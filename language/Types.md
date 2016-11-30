# Types

The type system defines the following types:

- Primitive Types: `Int`, `Number`, `String`, `Char`, `Boolean`
- Arrays 
- Records
- Tagged Unions
- Newtypes
- Functions
- Polymorphic Types
- Constrained Types
- Type Synonyms
- Rows

## Primitive Types

The primitive types `String`, `Number` and `Boolean` correspond to their Javascript equivalents at runtime.

## Integers

The `Int` type represents integer values. The runtime representation is also a normal JavaScript Number; however, operations like `(+)` on `Int` values are defined differently in order to ensure that you always get `Int` values as a result.

## Arrays

PureScript arrays correspond to Javascript arrays at runtime, but all elements in an array must have the same type.The `Array` type takes one type argument to specify what type this is. For example, an array of integers would have the type `Array Int`, and an array of strings would have the type `Array String`.

## Records

PureScript records correspond to Javascript objects. They may have zero or more named fields, each with their own types.

## Tagged Unions

Tagged unions consist of one or more constructors, each of which takes zero or more arguments.

Tagged unions can only be created using their constructors, and deconstructed through pattern matching (see later).

For example:

```purescript
data Foo = Foo | Bar String
  
runFoo Foo = "It's a Foo"
runFoo (Bar s) = "It's a Bar. The string is " <> s
  
test = runFoo Foo <> runFoo (Bar "Test")
```

In the example, Foo is a tagged union type which has two constructors. Its first constructor `Foo` takes no arguments, and its second `Bar` takes one, which must be a String.

`runFoo` is an example of pattern matching on a tagged union type to discover its constructor, and the last line shows how to construct values of type `Foo`.

## Newtypes

Newtypes are like data types (which are introduced with the `data` keyword), but are restricted to a single constructor which contains a single argument. Newtypes are introduced with the `newtype` keyword::

```purescript
newtype Percentage = Percentage Number
```

The representation of a newtype at runtime is the same as the underlying data type. For example, a value of type `Percentage` is just a JavaScript number at runtime.
  
Newtypes can be assigned their own type class instances, so for example, `Percentage` can be given its own `Show` instance:

```purescript
instance showPercentage :: Show Percentage where
  show (Percentage n) = show n <> "%"
```

## Functions

Functions in PureScript are like their Javascript counterparts, but always have exactly one argument.

## Polymorphic Types

Expressions can have polymorphic types:

```purescript
identity x = x
```

``identity`` is inferred to have (polymorphic) type ``forall t0. t0 -> t0``. This means that for any type ``t0``, ``identity`` can be given a value of type ``t0`` and will give back a value of the same type.

A type annotation can also be provided::

```purescript
identity :: forall a. a -> a
identity x = x
```

## Row Polymorphism

Polymorphism is not limited to abstracting over types. Values may also be polymorphic in types with other kinds, such as rows or effects (see "Kind System").

For example, the following function accesses two properties on a record::

```purescript
addProps o = o.foo + o.bar + 1
```
    
The inferred type of ``addProps`` is::

```purescript
forall r. { foo :: Int, bar :: Int | r } -> Number
```
  
Here, the type variable ``r`` has kind ``# *`` - it represents a `row` of `types`. It can be instantiated with any row of named types.

In other words, ``addProps`` accepts any record which has properties ``foo`` and ``bar``, and *any other record properties*.

Therefore, the following application compiles:

```purescript
addProps { foo: 1, bar: 2, baz: 3 }
```
    
even though the type of ``addProps`` does not mention the property ``baz``. However, the following does not compile:

```purescript
addProps { foo: 1 }
```
    
since the ``bar`` property is missing.

## Rank N Types

It is also possible for the ``forall`` quantifier to appear on the left of a function arrow, inside types record fields and data constructors, and in type synonyms.

In most cases, a type annotation is necessary when using this feature.

As an example, we can pass a polymorphic function as an argument to another function::

```purescript
poly :: (forall a. a -> a) -> Boolean
poly f = (f 0 < 1) == f true
```

Notice that the polymorphic function's type argument is instantiated to both ``Number`` and ``Boolean``.

An argument to ``poly`` must indeed be polymorphic. For example, the following fails::

```purescript
test = poly (\n -> n + 1)
```

since the skolemized type variable ``a`` does not unify with ``Int``.

## Rows

A row of types represents an unordered collection of named types without duplicates.

Rows have kind ``# k`` for some kind ``k``, and so values do not have types which are rows. Rather, rows can be used in type signatures to define record types or other type where labelled, unordered types are useful.

To denote a closed row, separate the fields with commas, with each label separated from its type with a double colon::

```purescript
(name :: String, age :: Number)
```

To denote an open row (i.e. one which may unify with another row to add new fields), separate the specified terms from a row variable by a pipe::

```purescript
(name :: String, age :: Number | r)
```

## Type Synonyms

For convenience, it is possible to declare a synonym for a type using the ``type`` keyword. Type synonyms can include type arguments.

For example::

```purescript
type Foo = { foo :: Number, bar :: Number }
  
addFoo :: Foo -> Number
addFoo o = o.foo + o.bar

type Bar a = { foo :: a, bar :: a }

combineBar :: forall a b. (a -> a -> b) -> Bar a -> b
combineBar f o = f o.foo o.bar
```
  
## Constrained Types

Polymorphic types may be predicated on one or more ``constraints``. See the chapter on type classes for more information.

## Type Annotations

Most types can be inferred (not including Rank N Types and constrained types), but annotations can optionally be provided using a double-colon::

```purescript
one = 1 :: Int

-- Either of the following will type check, because the first is a more general form of the second
x = one :: forall a. (Semiring a) => a
x = one :: Int
```

## Kind System

The kind system defines the following kinds:

- ``*``, the kind of types.
- ``!``, the kind of effects.
- Arrow kinds ``k1 -> k2``
- Row kinds ``# k``

## Row Kinds

The kind ``# k`` of rows is used to classify labelled, unordered collections of types of kind ``k``. 

For example ``# *`` is the kind of rows of types, as used to define records, and ``# !`` is the kind of rows of effects, used to define the monad ``Eff`` of extensible effects.

## Quantification

A type variable can refer to not only a type or a row, but a type constructor, or row constructor etc., and type variables with those kinds can be bound inside a ``forall`` quantifier.
