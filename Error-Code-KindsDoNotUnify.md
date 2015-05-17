This can happen when a function is declared without kind `*`, like a function that attempts to return a row:

```purs
onClick :: forall a. String -> (click :: String | a)
onClick s = {click: s}
```

The error message reads along the lines of:
```purs
  Prim.String -> (click :: Prim.String | a)

  Cannot unify kind
    *
  with kind
    # *
```

**Prime suspect:** You are missing an `->` in your type signature.