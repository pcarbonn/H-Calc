-- you need to adapt H-Calc.cabal to run this program: 
--  hs-source-dirs:     sandbox
--  main-is:            Sandbox.hs
-- + remove RebindableSyntax


module Sandbox where

  import Haskus.Utils.EADT
  import Prelude
  
  
  -- definition of the nodes in the AST : Val a i, EmptyNote, Typ t a
  -- examples: (Val EmptyNote 3)
  --           (Val (Typ TInt (EmptyNote)) 3)
  ----------------------------------------------------------

  -- a Val has an annotation 'a and a Int value 
  data ValF a = ValF a Int deriving (Functor)

  pattern Val :: ValF :<: xs => EADT xs -> Int -> EADT xs
  pattern Val a i = VF (ValF a i)

  -- at first, the annotation is empty
  data EmptyNoteF e = EmptyNoteF deriving (Functor)

  pattern EmptyNote :: EmptyNoteF :<: xs => EADT xs
  pattern EmptyNote = VF EmptyNoteF

  -- later, I want to prepend a Typ to the empty note

  data TTyp = TInt | TFloat deriving Show

  data TypF a = TypF TTyp a deriving (Functor)

  pattern Typ :: TypF :<: xs => TTyp -> EADT xs -> EADT xs
  pattern Typ t a = VF (TypF t a)

  -- Show
  ----------------------------------------------------------

  instance ShowEADT ValF where
    showEADT' (ValF a i) = show i <> " :: " <> a


  instance ShowEADT EmptyNoteF where
    showEADT' EmptyNoteF = "?"

  instance ShowEADT TypF where
    showEADT' (TypF t a) = show t

  -- boilerplate

  instance ShowEADT (VariantF '[]) where
    showEADT' = error "can't reach this point"

  class ShowEADT (f :: * -> *) where
    showEADT' :: f String -> String

  instance (ShowEADT x, ShowEADT (VariantF xs))  => ShowEADT (VariantF (x ': xs)) where
    showEADT' v = case popVariantFHead v of
      Right u -> showEADT' u
      Left  w -> showEADT' w

  showEADT :: (ShowEADT (Base t), Recursive t) => t -> String -- type inferred by GHC      
  showEADT = cata showEADT' 
       

  -- Transformation
  ----------------------------------------------------------
  -- I want to transform an AST1 into an AST2, by inserting Typ TInt in front of the annotation
  -- e.g. (Val EmptyNote 3) --> (Val (Typ TInt (EmptyNote)) 3)

  -- let's use a paramorphism


  class TypeAST (f :: * -> *) ys where
    typeAST' :: f (EADT ys, EADT ys) -> EADT ys -- first is original, 2nd is modified

  instance TypeAST (VariantF '[]) ys where
    typeAST' = error "no implementation of Type Check for this type"

  instance (TypeAST x ys, TypeAST (VariantF xs) ys)  => TypeAST (VariantF (x ': xs)) ys where
    typeAST' v = case popVariantFHead v of
        Right u -> typeAST' u
        Left  w -> typeAST' w


  instance (EmptyNoteF :<: ys) => TypeAST EmptyNoteF ys where
    typeAST' _ = EmptyNote

  instance (TypF :<: ys, EmptyNoteF :<: ys) => TypeAST TypF ys where
    typeAST' (TypF _ (_, s)) = s -- erase existing type  

  instance (EmptyNoteF :<: ys, ValF :<: ys, TypF :<: ys) => TypeAST ValF ys where
    typeAST' (ValF (a,_) i) = Val (Typ TInt a) i
     
  typeAST :: 
    ( TypeAST (VariantF xs) xs
    , Functor (VariantF xs)
    , TypF :<: xs
    ) => EADT xs -> EADT xs
  typeAST e = para typeAST' e 


  type AST1 = EADT '[ValF, EmptyNoteF]
  type AST2 = EADT '[ValF, EmptyNoteF, TypF]

  main :: IO ()
  main = do
    let ast1 = Val EmptyNote 1 :: AST1
    let ast2 = Val (Typ TInt EmptyNote) 2 :: AST2
    putStrLn $ showEADT ast1 -- 1 :: ?
    putStrLn $ showEADT $ typeAST $ appendEADT @'[TypF] ast1 -- TypeApplications
    putStrLn $ showEADT ast2 -- 2 :: TInt?