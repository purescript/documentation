# `InvalidNewtype` Error

## Example

```purescript
newtype NotIdentity a = a

newtype NotTuple a b = NotTuple a b
```

## Cause

Newtypes must define a single constructor with a single argument, so the two
most common sources of this error are having multiple arguments (like a `Tuple`)
or forgetting to provide a constructor.

## Fix

- If you need multiple arguments, consider using a newtype around a record or `Tuple`:

```purescript
newtype RecordTuple a b = RecordTuple {fst :: a, snd :: b}
```
