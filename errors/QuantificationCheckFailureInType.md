# `QuantificationCheckFailureInType` Error

## Example

```purescript
data Proxy :: forall k. k -> Type
data Proxy a = Proxy

type SomeProxy = forall a. Proxy a
```

## Cause

This error occurs when the compiler tries to implicitly generalize a kind but
there are multiple ways in which it could be quantified. This most often
happens in the right-hand-side of type synonyms. In the above example, what
is the kind of the type variable `a`?

```purescript
type SomeProxy = forall (a :: ???). Proxy a
```

We know that it could potentially be anything per the kind signature of
`Proxy`, but how should it be quantified?

```purescript
-- Quantifying in the forall.
type SomeProxy = forall k (a :: k). Proxy a

-- Quantifying in a kind signature
type SomeProxy :: forall k. Type
type SomeProxy = forall (a :: k). Proxy a

-- Quantify explicitly with an argument to the synonym
type SomeProxy k = forall (a :: k). Proxy a
```

The only places the compiler considers unambiguous are within top-level type
and kind signatures.

## Fix

- Explicitly quantify the kind in the way most appropriate for your use case.
