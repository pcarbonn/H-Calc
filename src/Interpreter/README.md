# File contents

- Interpreter.hs : main entry point of the interpreter
- Result.hs : defines the result of an interpretation
- Utils : defines the `HError Text` and `EmptyNote` AST nodes + some helpers
- A_TypeCheck : defines the `Typ t α` AST node used for type annotations
- B_Add : defines `Val α i`, `FloatVal α i` and `Add α (v1,v2)` AST nodes
- C_Mul : defines `Mul α (i,v)` AST nodes

# Transformations

Below is the list of AST transformations:

- showAST : a bottom up evaluation of the tree into a Text value
- getAnnotation : a direct evaluation of a tree into another tree
- getType : a top-down evaluation of a tree into a TType
- setType : a bottom up transformation of the tree to add type information in the annotation of each node.  The tree type must allow TType nodes.
- appendEADT @'[TTypeF] : to allow TType nodes in the tree
- distribute: fix point of a bottom up transformation using the distribution rule
- demultiply: bottom up transformation to replace multiplications by additions
- eval : a bottom up evaluation of the tree into a Result type


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