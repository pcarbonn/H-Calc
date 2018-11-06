WORK IN PROGRESS !  (See [TODO.md](https://github.com/pcarbonn/H-Calc/blob/master/TODO.md))

So, you want to write an interpreter for your own Domain Specific Language (DSL), in Haskell...  Then, clone H-Calc and start editing it.  H-Calc is a showcase of some of the best Haskell technologies to write a DSL interpreter, showing you that you can :

- parse source code in your DSL with ease, and build the corresponding Abstract Syntax Tree (AST), thanks to [megaparsec](http://hackage.haskell.org/package/megaparsec) ([tutorial](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html))
- start with a simple interpreter for the core features of your DSL, then add extensions in a modular way (each new feature in its own Haskell module), thanks to [Extensible Algebraic Datatype](https://hackage.haskell.org/package/haskus-utils-variant) (EADT)  ([doc](https://docs.haskus.org/eadt.html))
- transform the AST efficiently, without boilerplate, thanks to [Recursion-scheme](http://hackage.haskell.org/package/recursion-schemes-5.0.3) ([tutorial](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/))
- embed your DSL in Haskell, thanks to [RebindableSyntax](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide)
- easily use the best functions and libraries of Haskell, thanks to [Relude](http://hackage.haskell.org/package/relude)<sup>[1](#myfootnote1)</sup>
- organize your test suite, thanks to [HSpec](http://hackage.haskell.org/package/hspec), with [hspec-discover](http://hackage.haskell.org/package/hspec-discover)
- automatically run your test suite as soon as you save a program file, provided it has no error, thanks to [Ghcid](https://github.com/ndmitchell/ghcid)

H-Calc is a toy example, a calculator that can add integers or floats.  It also supports multiplication by repeated additions: it transforms `n*a` into `a+a+...` (n times), after applying the distribution rule recursively: `a*(b+c) = a*b + a*c`.  `n` must be a positive integer.  

    2 * (3 + 4) -> ((2 * 3) + (2 * 4)) -> ((3 + 3) + (4 + 4)) -> 14

The evaluation pipeline has these steps:

* parse the text containing the formula into the AST
* annotate the AST with type information, and report any type error
* transform the AST in several "nanopass"
* finally evaluate the simplified AST to get the desired result

# Get started

* Install [Stack](haskellstack.org)
* clone H-Calc from github to a directory on your machine
* go to that directory
~~~~
dir> stack build
dir> stack install ghcid
~~~~
To automatically run Main.hs whenever your program changes:
~~~~
dir> ghcid --command="stack ghci H-Calc" --test="main"
~~~~
To automatically run the test suite whenever your program changes:
~~~~
dir> ghcid --command="stack ghci H-Calc:lib H-Calc:spec" --test="main"
~~~~

# An introduction to Extensible ADT (EADT)

## Abstract Syntax Tree

Our abstract syntax tree has the following core types of nodes, defined using recursive data constructors ([tutorial](https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/)):

    data ValF      e =      ValF e Int
    data FloatValF e = FloatValF e Float 
    data AddF      e =      AddF e (e, e)

We'll also have a node to represent an error detected when interpreting an H-Calc formula.
    
    data HErrorF e = HErrorF e Text 

The first data element of each constructor will contain some annotations, and the second will contain the arguments.  Annotations are represented as a chain of annotation nodes.  The simplest annotation node is `EmptyNote`; another annotation node will contain the data type of the node (Int or Float).  

    data EmptyNoteF e = EmptyNoteF
    
    data TType = TInt | TFloat
    data TypF e = TypF e TType  
  
To facilitate the creation and handling of nodes, the haskus-utils package proposes the `eadtPattern` function:

    eadtPattern 'ValF      "Val"
    eadtPattern 'FloatValF "FloatVal"
    eadtPattern 'AddF      "Add"
    
    eadtPattern 'EmptyNoteF "EmptyNote"
    eadtPattern 'TypF "Typ"
    eadtPattern 'HErrorF "HError"
  
With this apparatus, we can now represent `1` and `(1+2) :: Int` with the following formula:

    one :: EADT '[EmptyNoteF, ValF]
    one = Val EmptyNote 1
    
    oneTwo :: EADT '[EmptyNoteF, ValF, AddF, TypF]
    oneTwo = Add (Typ EmptyNote TInt) (Val EmptyNote 1, Val EmptyNote 2)

The first line declares the types of nodes used in the AST.  The second line constructs the various nodes of our tree representation.

We can easily extend what our trees can represent by adding new types of nodes, for example for a multiplication:

    data MulF      e =      MulF e (e, e)
    eadtPattern 'MulF      "Mul"
    
    mulTwo :: EADT '[EmptyNoteF, ValF, MulF]
    mulTwo = Mul EmptyNote (Val EmptyNote 2, Val EmptyNote 1)


## Algebra

We can now define various algebra on our recursive data types, i.e. various functions to evaluate our tree to a fixed data type.  For example, `showAST` is a function to evaluate our tree as a Text value.

    showAST :: EADT xs -> Text
  
Some algebra are extensible, tree-wide transformations: as new types of nodes are added, new definitions are required.  To do that, we use `Class` and `instance`:

    class Algebra (f :: * -> *) where
      showAST' :: f Text -> Text
      -- add your algebra declaration here

    -- (additional code not shown for clarity. See Transfos.hs)

    showAST = cata showAST'


    instance Algebra AddF where
      showAST' (AddF α (v1,v2)) = "(" <> v1 <> " + " <> v2 <> ")" <> α    

`cata` is a function for catamorphism, from the recursion-scheme package.  (`ShowAST` could also be implemented without it)  See the source code for the instances of `showAST'` for the other node types.

The `eval` function is another algebra, from AST tree to Result.

    data Result 
      = RInt Int
      | RFloat Float
      | RError Text
    
`eval` is NOT supposed to be extensible: it accepts only a limited set of tree nodes.  Trees with other types of nodes will have to be simplified before being evaluated (see below).  For such non-extensible algebra, we define how to interpret each type of node using lambdas, called "continuations" (see Interpreter.hs):

    eval :: EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF] -> Result
    eval l = eadtToCont l >::>
        ( \(HErrorF _ t)    -> RError t
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

Often, you want to enrich a tree with some annotations, for example to add the data type of each sub-tree.  To expand the capability of a tree, use `appendEADT @'[newNodeConstructor]`:

    one' = appendEADT @'[TypF] one :: EADT '[ValF, TypF]

You can then fill the type using an isomorphism, i.e. a transformation from `EADT xs` to `EADT xs`: the list of possible nodes does not change, but the structure of the tree may very well.  Again, when we want to have an extensible, tree-wide isomorphism, we'll use `Class` and `instance` (see Transfos.hs):

    class Isomorphism xs (f :: * -> *) where
      getAnnotation :: f (EADT xs) -> EADT xs
      setType' :: f (EADT xs) -> EADT xs
      -- add more isomorphisms here

    setType = cata setType'
    
Here is the isomorphism definition for Val (see source code for more):

    instance ('[TypF, ValF] :<<: xs) => Isomorphism xs ValF where
      getAnnotation (ValF α _) = α
      setType' (ValF α i) = Val (Typ α TInt) i

When the isomorphism is not extensible and tree-wide (i.e. it requires no new definition for new types of nodes) you should use continuations.  This is the case when you want to apply the distributivity rule on the tree (C_Mul.hs):

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

One of the case branch defines a default implementation for the other types of node: just transform their children.

      
## Tree reduction

Another type of transformation you want to make on your tree is to reduce the list of possible types of nodes.

For example, at some point, you'll want to remove the annotation, i.e. replace all of them by `EmptyNote`.  When the reduction is tree-wide, use `Class` and `instance` (see Transfos.hs):

    class RemoveAnnotation ys (f :: * -> *) where
      removeAnnotation'      :: f (EADT ys) -> EADT ys
      
    removeAnnotation = cata removeAnnotation'
    
    -- (additional code omitted)

    instance (EmptyNoteF :<: xs) => RemoveAnnotation xs TypF where
      removeAnnotation' (TypF _ _) = EmptyNote

You can define a default instance implementation, e.g. for Add, Mul, Sub, using `{-# OVERLAPPABLE #-}`:
  
     -- if the type of node is in the result type, keep it as is 
    instance {-# OVERLAPPABLE #-} f :<: ys => RemoveAnnotation ys f where
      removeAnnotation' = VF

If instead, the tree reduction is not extensible and tree-wide, i.e. it transforms one type of node only, use continuations, as in `demultiply`:

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
 

You could further add support for the following features:
- keep track of the location of error in the source text;
- evaluate `n` as `1+1+...` (n times);
- support `let x = expr1 in expr2`;

Feel free to give it a try !

(See [Interpreter.README](https://github.com/pcarbonn/H-Calc/tree/master/src/Interpreter) for some additional technical note on the implementation).


# Footnote


<a name="myfootnote1">1</a>: GHC comes with a set of libraries ("[base](http://hackage.haskell.org/package/base)"), some of which are imported by default in your program ("[Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html)").  For historical reason, some choices that Prelude made are not optimal anymore.  Relude is an alternative Prelude that uses Text instead of String and avoids partial functions, among other benefits.
