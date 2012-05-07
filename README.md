HaskSymb: An Experiment in Haskell Symbolic Algebra
===================================================

At this point, HaskSymb is just something I'm hacking on as a fun side project. It is not a serious project, and isn't useful for anything. That said, it has some cool features.

The biggest one is that it provides mathematical pattern matching via quasiquoters. So one can write code like this:

```haskell
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand [m|  a*b  |] = expand a * expand b
expand       a      = a
```

(In fact, at the time of writing that is actual code in BasicAlgs)

There's other cool stuff with extensibility coming up.
