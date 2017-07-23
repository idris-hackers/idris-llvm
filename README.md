# idris-llvm

This is a LLVM backend for Idris.

## Installing

Idris-llvm uses llvm-hs to bind to LLVM, it requires that a recent LLVM (at the moment LLVM 4.0) is installed in a location that GHC knows about. Required C libraries are the Boehm GC (it could be called "libgc" or "gc") and GMP.

If the prerequisites are met `cabal install` should be sufficient to build and install idris-llvm.

## Usage

At the moment idris-llvm outputs textual LLVM files that need to be manually built and linked.

How to build an executable:
```
idris myprog.idr --codegen llvm -o myprog.ll
llc myprog.ll
clang myprog.s src/rts/libidris_rts.a -lgc -lgmp
```

This procedure is temporary, idris-llvm will do all the work soon.
