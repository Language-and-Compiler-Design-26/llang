# This folder contains the OCaml implementation of the llang compiler/interpreter.

Run 

```bash
dune exec llang
```

to execute the llang interpreter, which presents a REPL for the llang language. The output of the REPL is a representation of the abstract syntax tree (AST), the result of the evaluation and the llvm code generated from the input llang code.