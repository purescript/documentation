This error occurs when the type checker is unable to deduce that two types are 'the same'.

For example, take this expression:

```purescript
[] == [unit]
```

`[]` has type `forall a. Array a`, but `[unit]` has type `Array Unit`. These types are not the same. But the type checker is able to determine that the type `Unit` may be chosen for the type variable `a`, so the types do become the same. We say that the type checker successfully unifies `forall a. Array a` with `Array Unit` in this case.

Another example:

```purescript
f :: Number -> Number
f x = x + 1
g :: Boolean -> Boolean
g x = x || true

h = g <<< f
```

The type of `(<<<)` (that is, function composition) is `forall a b c. (b -> c) -> (a -> b) -> (a -> c)`. For the right hand side of `h` to type-check, we need to find types `a`, `b`, and `c` such that the types match up. That is, we need to find a choice of `a`, `b`, and `c` such that:

* `b = Boolean` (from the argument type of `g`)
* `c = Boolean` (from the return type of `g`)
* `a = Number` (from the argument type of `f`)
* `b = Number` (from the return type of `f`).

`b` can not be `Boolean` and `Number` at the same time, so this system of equations is not satisfiable, and the type checker rejects the program.

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
  has type

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

### Eff <-> Function

If you get this error message:

    Could not match type

      Eff

    with type

      Function

... then check to see whether you might have written a "do" block but forgotten to actually say `do`.

If, on the other hand, the message has the types reversed:

    Could not match type

      Function

    with type

      Eff

Then you may have neglected to provide all the required arguments in one of the lines of a do-statement, leaving a function that takes the missing args, rather than the result type of Eff.

### Removing effect rows
You may see this error if all effect rows have been removed (using `Control.Monad.Eff.Exception.catchException` or `Control.Monad.Eff.Exception.try` for example). You can use `Control.Monad.Eff.runPure` to safely remove the `Eff`.
