# An introduction to Extensible ADT (EADT)

## Abstract Syntax Tree

Our abstract syntax tree has the following core types of nodes, defined using recursive data constructors ([tutorial](https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/)):

    data ValF      e =      ValF e Int
    data FloatValF e = FloatValF e Float 
    data AddF      e =      AddF e (e, e)
    data MulF      e =      MulF e (e, e)

The first data element of each constructor will contain some annotations, and the second will contain the arguments.  The simplest annotation is `EmptyNote`; another annotation will contain the data type of the node (Int or Float).  We'll also have a node to represent an error detected when interpreting an H-Calc formula.

    data EmptyNoteF e = EmptyNoteF
    
    data TType = TInt | TFloat
    data TypF e = TypF e TType  
    
    data HErrorF e = HErrorF Text 
  
To facilitate the creation and handling of nodes, the haskus-utils package proposes the eadtPattern function:

    eadtPattern 'ValF      "Val"
    eadtPattern 'FloatValF "FloatVal"
    eadtPattern 'AddF      "Add"
    
    eadtPattern 'EmptyNoteF "EmptyNote"
    eadtPattern 'TypF "Typ"
    eadtPattern 'HErrorF "HError"
  
With this apparatus, we can now represent 1 and (1+2) with the following formula:

    one :: EADT '[EmptyNoteF, ValF]
    one = Val EmptyNote 1
    
    oneTwo :: EADT '[EmptyNoteF, ValF, AddF, TypF]
    oneTwo = Add (Typ EmptyNote TInt) (Val EmptyNote 1, Val EmptyNote 2)

The first line declares the types of nodes used in the AST.  The second line constructs the various nodes of our tree representations.

We could easily extend what our trees can represent by adding new types of nodes, for example for a substraction:

    data SubF      e =      SubF e (e, e)
    eadtPattern 'SubF      "Sub"
    
    minTwo :: EADT '[EmptyNoteF, ValF, SubF]
    minTwo = Sub EmptyNote (Val EmptyNote 2, Val EmptyNote 1)

## Algebra

We can now define various algebra on our recursive data types, i.e. various functions to evaluate our tree to a fixed data type.  For example, `showAST` is a function to evaluate our tree as a Text value.

    showAST :: EADT xs -> Text
  
Some algebra definition should be extensible, to accomodate the new types of nodes that we may want to use later.  To accomplish that, we use `Class` and `instance`:

    class Algebra (f :: * -> *) where
      showAST' :: f Text -> Text
      -- add more algebra here

    showAST = cata showAST'
    

    instance Algebra AddF where
      showAST' (AddF α (v1,v2)) = "(" <> v1 <> " + " <> v2 <> ")" <> α    

`cata` is a function for catamorphism, from the recursion-scheme package.  `ShowAST` could also be implemented without it.  See the source code for the other instances of `showAST'`.

The `eval` function is another algebra, from AST tree to Result.

    data Result 
      = RInt Int
      | RFloat Float
      | RError Text
    
`eval` is NOT supposed to be extensible: it accepts only a limited set of tree nodes.  Trees with other types of nodes will have to be simplified before being evaluated (see below).  For such non-extensible algebra, we define how to interpret each type of node using eadtToCont (see Interpreter.hs):

    eval :: EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF] -> Result
    eval l = eadtToCont l >::>
        ( \(HErrorF t)      -> RError t
        , \(EmptyNoteF)     -> RError "can't evaluate empty expression"
        , \(ValF _ i)       -> RInt i
        , \(FloatValF _ f)  -> RFloat f
        , \(AddF _ (v1,v2)) -> go (eval v1) (eval v2))
        where 
          go v1 v2 =
            case (v1, v2) of -- implicit recursion
              (RInt v1', RInt v2')     -> RInt (v1'+v2')
              (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
              (RError e, _) -> RError e
              (_, RError e) -> RError e
              (a,b)         -> RError $ "Error in eval(" <> show a <> "+" <> show b <> ")"

## Expansions and isomorphisms

Sometimes, you want to enrich a tree with some annotations, for example to add the data type of each sub-tree.  To expand the capability of a tree, use `appendEADT @'[newNodeConstructor]`:

    one' = appendEADT @'[TypF] one :: EADT '[ValF, TypF]

You can then fill the type using an isomorphism, i.e. a transformation from `EADT xs` to `EADT xs`: the list of possible nodes does not change, but the structure of the tree may very well.  Again, if we want to have an extensible isomorphism, we'll use `Class` and `instance` (see Transfos.hs):

    class Isomorphism xs (f :: * -> *) where
      getAnnotation :: f (EADT xs) -> EADT xs
      setType' :: f (EADT xs) -> EADT xs
      -- add more isomorphisms here

    setType = cata setType'
    
Here is the isomorphism definition for Val (see source code for more):

    instance ('[TypF, ValF] :<<: xs) => Isomorphism xs ValF where
      getAnnotation (ValF α _) = α
      setType' (ValF α i) = Val (Typ α TInt) i

Again, if the isomorphism is not meant to be extensible, you should use continuations.  This is the case when you want to apply the distributivity rule on the tree (C_Mul.hs):

    -- apply distribution : a*(b+c) -> (a*b+a*c)
    distribute x = case splitVariantF @'[AddF, MulF] $ unfix x of
      Right leftovers -> leftovers & (fmap d) & liftVariantF & Fix
      Left v          -> variantFToCont v >::>
                            ( \(AddF α (v1,v2)) -> Add α (v1,v2) --TODO
                            , \(MulF α (v1,v2)) -> go α (v1,v2)
                            )
      where
        d v = distribute v
        go α (i, (Add β (v1,v2))) = Add β (d (Mul α (i,d v1)), d (Mul α (i,d v2)))
        go α ((Add β (v1,v2)), i) = Add β (d (Mul α (d v1,i)), d (Mul α (d v2,i)))
        go α (v1,v2)              = Mul α (d v1, d v2)


      
## Tree reduction

Another type of operation you want to make on your tree is to reduce the list of possible types of nodes.

For example, at some point, you'll want to remove the annotation, i.e. replace all of them by `EmptyNote`.  When the reduction should be extensible, use `Class` and `instance` (see Transfos.hs):

    class RemoveAnnotation ys (f :: * -> *) where
      removeAnnotation'      :: f (EADT ys) -> EADT ys
      
    instance (EmptyNoteF :<: xs) => RemoveAnnotation xs TypF where
      removeAnnotation' (TypF _ _) = EmptyNote

You can generically define a default implementation, e.g. for Add, Mul, Sub:
  
     -- if the type of node is in the result type, keep as is 
    instance {-# OVERLAPPABLE #-} f :<: ys => RemoveAnnotation ys f where
      removeAnnotation' = VF    

If instead, the tree reduction is not extensible, use continuations, as in `demultiply`:

    --TODO : TypF
    demultiply x = case splitVariantF @'[MulF, TypF] $ unfix x of
      Right leftovers -> leftovers & (fmap d) & liftVariantF & Fix
      Left v          -> variantFToCont v >::>
                            ( \(MulF α (v1,v2)) -> go α (v1,v2)
                            , \(TypF α t) -> Typ (d α) t --TODO
                            )
      where 
        d v = demultiply v -- in target AST
        go α (v1,v2) = 
          let 
            go' i v v' = 
                  if  | i < 0 -> HError $ "Error: can't multiply by negative number " 
                                <> show i
                      | i == 0 -> Val (d α) 0
                      | i == 1 -> v'
                      | otherwise -> Add (d α) (d v, d $ Mul α ((Val α $ i-1), v))
          in
            case (d v1, d v2) of
              (HError e, _) -> HError e
              (_, HError e) -> HError e
              (Val _ i1, v2') -> go' i1 v2 v2'
              (v1', Val _ i2) -> go' i2 v1 v1'
              (_, _) -> HError $ "Can't multiply by " <> showAST v1
 