# H-Calc
So, you want to write a compiler or interpreter for your own Domain Specific Language (DSL) ?  Then, clone H-Calc and start editing it.  H-Calc is a showcase of the best Haskell technologies to write such a compiler :

- [Extensible Algebraic Datatype](http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html) (EADT): start with a simple DSL compiler for the core features, then add extensions in a modular way (each new feature in its own Haskell module).
- [megaparsec](http://hackage.haskell.org/package/megaparsec): a library to parse source code in your DSL, and build the corresponding Abstract Syntax Tree (AST)
- [Relude](http://hackage.haskell.org/package/relude): GHC comes with a set of libraries ("[base](http://hackage.haskell.org/package/base)"), some of which are imported by default in your program ("[Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html)").  For historical reason, some choices that Prelude made are not optimal anymore.  Relude is an alternative Prelude that uses Text instead of String and avoids partial functions, among other benefits.
- (TODO) Test suite
- [Ghcid](https://github.com/ndmitchell/ghcid): Automatically run your test suite as soon as you save your file, provided it has no error nor warning (using `ghcid "--command=stack ghci H-Calc" "--test=:main"`)