WORK IN PROGRESS !  (See [TODO.md](https://github.com/pcarbonn/H-Calc/blob/master/TODO.md))

So, you want to write an interpreter for your own Domain Specific Language (DSL), in Haskell...  Then, clone H-Calc and start editing it.  H-Calc is a showcase of some of the best Haskell technologies to write a DSL interpreter, showing you that you can :

- parse source code in your DSL with ease, and build the corresponding Abstract Syntax Tree (AST), thanks to [megaparsec](http://hackage.haskell.org/package/megaparsec) ([tutorial](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html))
- start with a simple interpreter for the core features of your DSL, then add extensions in a modular way (each new feature in its own Haskell module), thanks to [Extensible Algebraic Datatype](http://hsyl20.fr/home/posts/2018-05-22-extensible-adt.html) (EADT)  ([tutorial](https://docs.haskus.org/eadt.html)) 
- transform the AST efficiently, without boilerplate, thanks to [Recursion-scheme](http://hackage.haskell.org/package/recursion-schemes-5.0.3) ([tutorial](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/))
- embed your DSL in Haskell, thanks to [RebindableSyntax](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide)
- easily use the best functions and libraries of Haskell, thanks to [Relude](http://hackage.haskell.org/package/relude)<sup>[1](#myfootnote1)</sup>
- organize your test suite, thanks to [HSpec](http://hackage.haskell.org/package/hspec), with [hspec-discover](http://hackage.haskell.org/package/hspec-discover)
- automatically run your test suite as soon as you save a program file, provided it has no error, thanks to [Ghcid](https://github.com/ndmitchell/ghcid)

The evaluation pipeline has these steps:

* parse the text containing the formula into the AST
* annotate the AST with type information, and report any type error
* transform the AST in several "nanopass"
* finally evaluate the AST to get the desired result

# Get started

* (Install Haskell Plaftorm)
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



# H-Calc

To illustrate the use of these technologies, H-Calc is a (limited) calculator that accepts formula containing additions and multiplications, and that evaluates them using additions only. 

The first iteration of our DSL interpreter supports the addition of integers (or floats) only.  This requires some type checking.

    2 + 3 -> 5
    2.0 + 3.0 -> 5.0
 

A second iteration supports multiplication by repeated additions: it transforms `n*a` into `a+a+...` (n times), after applying the distribution rule recursively: `a*(b+c) = a*b + a*c`.  `n` must be a positive integer.   This new functionality is implemented in C_Mul.hs only, showing the modularity of the design. 

    2 * (3 + 4) -> ((3 + 3) + (4 + 4)) -> 14

Yet another iteration could evaluate `n` as `1+1+...` (n times): feel free to give it a try !

(See [Interpreter.README](https://github.com/pcarbonn/H-Calc/tree/master/src/Interpreter) for some technical comments on the implementation).


# Footnote


<a name="myfootnote1">1</a>: GHC comes with a set of libraries ("[base](http://hackage.haskell.org/package/base)"), some of which are imported by default in your program ("[Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html)").  For historical reason, some choices that Prelude made are not optimal anymore.  Relude is an alternative Prelude that uses Text instead of String and avoids partial functions, among other benefits.