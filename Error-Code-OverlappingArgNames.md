This error occurs when you attempt to bind the same variable to different function arguments, e.g.

```purs
-- This type class instance throws an error.
-- Note 'b' is being bound twice
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty a b) (NonEmpty b d) = (a == b) && (b == d)
```

To further illustrate...
```purs
-- This code throws error
-- "Overlapping names in function/binder in declaration reverseAndCombine"
reverseAndCombine :: String -> String -> String
reverseAndCombine a a = (reverse a) ++ (reverse b)
  where
    reverse :: String -> String
    reverse val = foldr (\acc val -> val ++ acc)
                  ""
                  (Data.String.split "" val)
```

This error is mostly likely due to a coding typo.