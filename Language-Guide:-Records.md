# Language Guide: Records

Record literals are surrounded by braces, as in JavaScript:

```haskell
author :: { name :: String, interests :: [String] }
author =
    { name: "Phil"
    , interests: ["Functional Programming", "JavaScript"]
    }
```

Fields of records can be accessed using a dot, followed by the label of the field to access:

```
> author.name
"Phil"

> author.interests
["Functional Programming","JavaScript"]
```

## Kinds

`{ ... }` is just syntactic sugar for the `Object` type constructor, so `{ language:  String }` is the same as `Object ( language :: String )`.

The Object type constructor is parameterized by a row of types. In kind notation, `Object` has kind `# * -> *`. That is, it takes a row of types to a type.

`( language :: String )` denotes a row of types (something of kind `# *`), so it can be passed to `Object` to construct a type, namely `Object ( language :: String )`.

## Extending Records

It is possible to define an extensible record

```haskell
type Lang l = { language :: String | l }
```

that can then be extended like:

```haskell
type Language = Lang ( country :: String )
```

## Wildcards

Record literals with wildcards can be used to create a function that produces the record instead:

```haskell
{ foo: _, bar: _ }
```
is equivalent to:

```haskell
\foo bar -> { foo: foo, bar: bar }
```
