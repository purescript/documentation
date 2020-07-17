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

Type synonyms for record types are created with the `type` keyword:

```purs
type Point =
  { x :: Number
  , y :: Number
  }
```

## Kinds

`{ ... }` is just syntactic sugar for the `Record` type constructor, so `{ language ::  String }` is the same as `Record ( language :: String )`.

The Record type constructor is parameterized by a row of types. In kind notation, `Record` has kind `# Type -> Type`. That is, it takes a row of types to a type.

`( language :: String )` denotes a row of types (something of kind `# Type`), so it can be passed to `Record` to construct a type, namely `Record ( language :: String )`.

## Extending Records

It is possible to define an extensible record

```purescript
type Lang l = { language :: String | l }
```

that can then be extended like:

```purescript
type Language = Lang ( country :: String )
```

The `Language` type synonym would then be equivalent to `{ language :: String, country :: String }`. Note that parentheses must be used for the extension, since `l` has to be a row kind not a record type.

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

This can be used to update nested records:

```purescript
setPersonPostcode :: PostCode -> Person -> Person
setPersonPostcode pc p = p { address { postCode = pc } }
```

A record update function can also be defined by using an `_` inplace of the record to be updated like:

```purescript
_ { fieldName = newValue }
```

## Record Puns

_Record puns_ enable concise code when record fields have the same name as other values. This feature is useful for constructing records and pattern matching.

### Constructing

```purs
origin :: Point
origin = { x, y }
-- origin = { x: x, y: y } -- Equivalent
  where
    x = 0.0
    y = 0.0
```

### Pattern Matching

```purs
showPoint :: Point -> String
showPoint { x, y } = show x <> ", " <> show y
-- showPoint { x : x, y : y } = show x <> ", " <> show y -- Equivalent
```

### Not for Record Updates

Note that puns may not be used for record updates.

```purescript
setX :: Number -> Point -> Point
setX x point = point { x = x }
-- setX x point = point { x } -- Not allowed
```

## Merging Records

The [`record`](https://pursuit.purescript.org/packages/purescript-record) package enables additional record operations, such as `merge` and `union`.

Here's an example of using the `disjointUnion` function to add a `z` field to a `Point` record. The `to3d` function merges the original record `p` with another record `{ z }` created from this new `Number`:

```purescript
import Record (disjointUnion)

type Point3d
  = { x :: Number
    , y :: Number
    , z :: Number
    }

to3d :: Point -> Number -> Point3d
to3d p z = disjointUnion p { z }

-- Equivalent to:
to3d p z =
  { x: p.x
  , y: p.y
  , z
  }
```

## Field Names

Symbols which are illegal value identifiers, such as title-cased identifiers or ones containing spaces, can be used to identify a field by enclosing it in double-quotes:

```purescript
author' :: { "Name" :: String, "Personal Interests" :: Array String }
author' = { "Name": "Phil", "Personal Interests": ["Functional Programming", "JavaScript"] }

> author'."Name"
"Phil"

> (author' { "Name" = "John" })."Name"
"John"
```

If compiling to JavaScript, consider that the PureScript compiler will allow you to choose symbols which have special meaning in JavaScript, like `__proto__`.

```purescript
oops = {__proto__: unsafeCoerce ""}.__proto__.constructor.constructor "alert('lol')" 0
-- When loaded onto a web page, this will display "lol" in a browser dialog box,
--   which is an effectful behavior despite this expression appearing to be pure.
```
