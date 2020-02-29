# `QuantificationCheckFailureInKind` Error

## Cause

This error occurs when implicitly generalizing the kind of a `forall`
quantified type variable would result in an ill-scoped generalization. The
compiler always orders implicitly generalized kind variables first within a
`forall` quantifier. It's possible to introduce a dependency on an explicit
type variable in an inferred kind such that generalizing that kind would
place it out of the scope of its dependency.

## Fix

- Generalize and order the kind explicitly
