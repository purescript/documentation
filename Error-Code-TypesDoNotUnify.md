### How to understand this error

```
Error found:
in module Main
at src/Main.purs line 51, column 9 - line 51, column 9

  Could not match type

    [A]

  with type

    [B]


while trying to match type [C]
  with type

     [D]

while checking that expression [E]   <-- Pay close attention here!
  has type Eff

     [F]

in value declaration [...]
```

In general, **[A]** is the type that was calculated for the expression in question—the **type you have**, and **[B]** is the type that was demanded by the context (an erroneous type signature perhaps, or the type signature of a non-matching function argument, etc)—the **type you need**.

**[C]** is again the "type you have", but with a little more detail (a step outside the immediate problem found), with **[D]** again being the "type you need".

Pay very close attention to **[E]**, because this time it's not a type, but an expression, and **often the direct source of the trouble**. It's easy to miss, because it's kind of buried in there with the rest of the text. **[F]** is the type of that expression.

### Matching effect names

```
Error checking that type
  Control.Monad.Eff.Eff (assert :: Test.Assert.ASSERT | _0) Prelude.Unit
subsumes type
  Control.Monad.Eff.Eff (a :: Test.Assert.ASSERT | e0) Prelude.Unit
Error at /.../Main.purs line 7, column 3 - line 7, column 10:
  Cannot unify type
    (assert :: Test.Assert.ASSERT | _0)
  with type
    (a :: Test.Assert.ASSERT | e0)
```

Even though the type of the effect (`Test.Assert.ASSERT`) is correct, the name does not match. The effect name *must* match. So to fix this error, change `a` to `assert`. 

*When using Halogen*

If you are seeing this error in relation to the definition of a component try checking the type signatures  of the component and child component's slot addresses, query types and state types against the examples in the purescript halogen repository.

### Eff <-> Function

If you get this error message:

    Could not match type

      Eff

    with type

      Function

... then check to see whether you might have written a "do" block but forgotten to actually say `do`.

If, on the other hand, the message has the types reversed:

    Could not match type

      Eff

    with type

      Function

Then you may have neglected to provide all the required arguments in one of the lines of a do-statement, leaving a function that takes the missing args, rather than the result type of Eff.