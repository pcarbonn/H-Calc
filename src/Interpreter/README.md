# File contents

- Interpreter.hs : main entry point of the interpreter
- Result.hs : defines the result of an interpretation
- Utils : defines the `HError Text` and `EmptyNote` AST nodes + some helpers
- A_Type : defines the `Typ t α` AST node used for type annotations
- B_Add  : defines `Val α i`, `FloatVal α i` and `Add α (v1,v2)` AST nodes
- C_Mul  : defines `Mul α (i,v)` AST nodes

# Transformations

When a transformation is generic, i.e. can be applied to AST of different types, we use `Class` and `instance`:
- to transform an AST tree into a fixed type (Algebra), e.g. `showAST`;
- to transform an AST tree into another tree of the same type (isomorphism), e.g. `getAnnotation`, `setType`;
- to transform an AST tree into a simpler one (reduction), e.g. `removeAnnotation`, `demultiply`.

When a transformation can be applied to only one type of tree, we prefer to use continuations:
- to transform AST into a result, i.e. `eval`.



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