# Records

Record literals are surrounded by braces, as in JavaScript:

```purescript
author :: { name :: String, interests :: Array String }
author =
    { name: "Phil"
    , interests: ["Functional Programming", "JavaScript"]
    }
```

Fields of records can be accessed using a dot, followed by the label of the field to access:

```purescript
> author.name
"Phil"

> author.interests
["Functional Programming","JavaScript"]
```

## Kinds

`{ ... }` is just syntactic sugar for the `Record` type constructor, so `{ language ::  String }` is the same as `Record ( language :: String )`.

The Record type constructor is parameterized by a row of types. In kind notation, `Record` has kind `# * -> *`. That is, it takes a row of types to a type.

`( language :: String )` denotes a row of types (something of kind `# *`), so it can be passed to `Record` to construct a type, namely `Record ( language :: String )`.

## Extending Records

It is possible to define an extensible record

```purescript
type Lang l = { language :: String | l }
```

that can then be extended like:

```purescript
type Language = Lang ( country :: String )
```

The `Language` type synonym would then be equivalent to `{ language :: String, country :: String }`.

## Wildcards

Record literals with wildcards can be used to create a function that produces the record instead:

```purescript
{ foo: _, bar: _ }
```
is equivalent to:

```purescript
\foo bar -> { foo: foo, bar: bar }
```

## Record Update

PureScript also provides a record update syntax similar to Haskell's:

```purescript
setX :: Number -> Point -> Point
setX val point = point { x = val }
```
