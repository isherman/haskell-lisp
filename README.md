Results of working through the [Write Yourself A Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) Haskell tutorial.

### To Run

- Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#the-haskell-tool-stack)

- Run the REPL
```
stack build
stack exec haskell-lisp-exe
```

- Load the "standard library"
```
Lisp>>> (load "stdlib.scm")
```

- You've got a Scheme!
```
Lisp>>> (map (curry + 2) '(1 2 3 4))
```

- Quit
```
Lisp>>> quit
```



