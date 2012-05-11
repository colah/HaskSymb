HaskSymb: An Experiment in Haskell Symbolic Algebra
===================================================

HaskSymb is a quickly hacked together proof of concept that I may expand at some point. It is not a serious project, isn't useful for anything, and has fairly ugly code. That said, it has some cool features.

The biggest one is that it provides mathematical pattern matching via quasiquoters. So one can write code like this:

```haskell
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand [m|  a*b  |] = expand a * expand b
expand       a      = a
```

The patterns match up to trivial mathematical equivalency. For example, because multiplication is commutative, `[m|a*(b+c)|]` matches `(x+1)*y` in addition to `2*(x+1)`. On the other hand, it doesn't attempt to do deeper analysis, like expanding expressions. It does support using the same variable multiple times (eg. `[m|a+a|]` which will match `x+x` but not `x+1`) and constants (eg. `[m|2*a|]`).

(In fact, at the time of writing that is actual code in *BasicAlgs*)

How does it work?
------------------

The basic idea is the *constructor-destructor* class pattern. For example, the class `SymbolicSum` is defined:

```haskell
class SymbolicSum a where
	sumC :: [a] -> a
	sumD ::  a  -> Maybe [a]
```

`sumC` constructs a value of type `a` representing a sum of `a`s. `sumD` attempts to destruct a value of type `a` into a list that could be summed into them.

Then, using View Patterns, we can write stuff like:

```haskell
foo (sumD -> Just vals) = "input is a sum of" ++ show vals
foo          _          = "input is not a sum"
```

Our quasiquoter, `m`, will build smarter destructors based off of these. Then if someone implements `SymbolicSum`, etc, they can use our patterns!

Where I'm Kind Of Stuck
------------------------

So this is a quickly hacked together proof of concept. The code is kind of ugly. Substantially, this is because I haven't figured out how to do things well.

The *big* issue I'm facing is appropriate types for symbolic expressions. In particular, how do I handle variables in types?

My ideal solution would involved dependent types, eg. `SymbolicExpr (Set [Var "x", Var "y"])`. This would allows all sorts of nice things like `set :: SymbolicExpr vars -> Var name -> Float -> SymbolicExpr (delete (Var name) vars)`, `(+) :: SymbolicExpr vars1 -> SymbolicExpr vars2 -> SymbolicExpr (union vars1 vars2)` and `eval :: SymbolicExpr (Set []) -> Float`. We could even give the variables types, eg. `SymbolicExpr (Set [Var "x" Float, Var "y" (Vector 3 Float)])`!

Sadly, I live in the real world and not fantasy land. (It may be possible to implement this with data promotion? I haven't done much with it yet. But I'm skeptical of it typechecking nicely...)

One thing I've considered is using GADTs as in [Dan Burton's really cool approach to implementing System F](https://github.com/DanBurton/Blog/blob/master/Literate%20Haskell/SystemF.lhs). But variables really shouldn't be ordered in this context; I really don't want to have something like `SymbolicExpr (Int -> Int -> Int)` for a symbolic expression with 2 integer variables, because there's no real reason for `x` or `y` to be the variable in first or second position. And for relationships between expressions, it would be the programmers job to relate positions, which is silly. It really needs to be a type-level thing.

My bad solution for now has been to just not have type-level variable representation, which kind of bothers me. And generally, when one part of a program is ugly, I find it difficult to motivate myself to make other parts pretty. :(

I'm planning to come back to this at some point, but I want to sit on it for a little while and think. Your feedback would be greatly appreciated; you can reach me at chris@colah.ca.


