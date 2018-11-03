# File contents

- Interpreter.hs : main entry point of the interpreter
- Transfos : declares AST-wide transformations: algebra, isomorphism and reductions
- A_Nucleus : defines the `HError Text`, `EmptyNote` and `Typ t α` AST nodes
- B_Add  : defines `Val α i`, `FloatVal α i` and `Add α (v1,v2)` AST nodes
- C_Mul  : defines `Mul α (i,v)` AST nodes

# Transformations

When a transformation is generic, i.e. can be applied to AST of different types, we use `Class` and `instance` (see Transfos.hs):
- to transform an AST tree into a fixed type (Algebra), e.g. `showAST`;
- to transform an AST tree into another tree of the same type (isomorphism), e.g. `getAnnotation`, `setType`;
- to transform an AST tree into a simpler one (reduction), e.g. `removeAnnotation`, `demultiply`.

When a transformation can be applied to only one type of tree, we prefer to use continuations:
- to transform an AST tree into a result, i.e. `eval` (in Interpreter.hs).



# A note on EADT

- `EADT '[HErrorF,EmptyNoteF, ValF,AddF]` is the type of an AST tree that contains only nodes constructed with `HError`, `EmptyNote`, `Val`, and `Add`.

- `FloatValF :<: xs` is a constraint on type `EADT xs` : `FloatVal` is one of the node constructors of the AST tree of type `EADT xs`.


# Naming conventions

- α : an annotation
- f : a float
- i : an integer
- s : text (string)
- t : a type
- v : an integer or float
- x : a node type
- y : a second node type
- ..1, ..2 : two different values
- ..s : plural