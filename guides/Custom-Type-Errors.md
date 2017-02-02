See the [24 days of PureScript 2016 post](https://github.com/paf31/24-days-of-purescript-2016/blob/master/21.markdown).

## The Warn type class

There is a type class in `Prim` called `Warn` - which, like `Fail` is indexed by a `Symbol`.
When the compiler solves a `Warn` constraint it will trivially solve the instance and print out the message as a user defined warning.

### Deprecation example

One possible use case for this is custom deprecation warnings.
You start with a library that exports a function `notBad`:

```purescript
notBad :: Int
notBad = 21
```

But now you decide there is something `better`; you want to deprecate `notBad` in favour of this new function:

```purescript
notBad :: Warn "`notBad` is deprecated. Prefer `better` instead." => Int
notBad = 21

better :: Int
better = 42
```

Now when someone uses `notBad`, they see the following compiler warning:

```
  A custom warning occurred while solving type class constraints:

    `notBad` is deprecated. Prefer `better` instead.
```
