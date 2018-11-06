
# File contents

- Interpreter.hs : main entry point of the interpreter
- Transfos : declares extensible transformations: algebra, isomorphism and reductions
- A_Nucleus : defines the `HError Text`, `EmptyNote` and `Typ t α` AST nodes
- B_Add  : defines `Val α i`, `FloatVal α i` and `Add α (v1,v2)` AST nodes
- C_Mul  : defines `Mul α (i,v)` AST nodes and associated transformations


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