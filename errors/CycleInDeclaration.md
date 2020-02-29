# `CycleInDeclaration` Error

## Example

```text
$ spago repl

> x = x
Error found:
at  line 1, column 5 - line 1, column 9

  The value of x is undefined here, so this reference is not allowed.


See https://github.com/purescript/documentation/blob/master/errors/CycleInDeclaration.md for more information,
or to contribute content related to this error.
```

## Cause

This error occurs when a value refers to itself, or another value in the same binding group, at a point where such a reference would be unavailable because of _strict evaluation_.

In the example above, it would be incorrect to generate the JavaScript

```javascript
var x = x;
```

since then `x` would be `undefined`.

Note that cycles can also spring up in much less obvious ways, e.g. if you define one typeclass member using another.

## Fix

- Consider the problem again, keeping PureScript's strict evaluation in mind. How would you solve this problem in JavaScript, for example?
- Sometimes you can break the cycle by strategically using eta expansion (`\x -> f x` instead of `f`) or using do-notation, which will introduce a function to the same effect because of `bind`.
- You might be able to make use of the `Control.Lazy` module, and the [`fix`](https://pursuit.purescript.org/packages/purescript-control/4.1.0/docs/Control.Lazy#v:fix) function in particular.

## Notes

- If you are trying to implement a recursive parser in PureScript, then you might find this post on [recursive parsing in PureScript](https://github.com/Thimoteus/SandScript/wiki/2.-Parsing-recursively) to be helpful.
