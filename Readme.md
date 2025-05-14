### Build the Compiler

```
ocamlbuild -pkgs llvm poetlang.native
```

### Run the compiler and generate llvm code
```
./poetlang.native -l example.mc > example.out
```

### Run the llvm code
```
lli example.out
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST) definition
-  `scanner.mll`: scanner
-  `parser.mly`: parser
-  `sast.ml`: definition of the semantically-checked AST
-  `semant.ml`: semantic checking
-  `irgen.ml`: LLVM IR code generator

### Other files

- `example.mc`: a sample source code that should not work since it doesn't rhyme
- 'example2.mc': a sample source code of gcd with rhyme scheme that works
- 'rhyme_example.mc': a sample source code with no main func that should work with rhyme scheme
- `example.out`: a sample compiled code
