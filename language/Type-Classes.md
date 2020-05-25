# Type Classes

PureScript supports type classes via the `class` and `instance` keywords.

Types appearing in class instances must be of the form `String`, `Number`, `Boolean`, or `C t1 ... tn` where `C` is a type constructor (including `->` and `t_i` are types of the same form).

Here is an example of the `Show` typeclass, with instances for `String`, `Boolean` and `Array`:

```purescript
class Show a where
  show :: a -> String

instance showString :: Show String where
  show s = s

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showArray :: (Show a) => Show (Array a) where
  show xs = "[" <> joinWith ", " (map show xs) <> "]"

example = show [true, false]
```

Overlapping instances are no longer allowed in PureScript. To write overlapping instances, you should use Instance Chains.

## Instance Chains

PureScript implements a form of instance chains that work on groups of instances matching by parameters. This means that constraints are not considered when choosing instances. However, you can still write a chain of instances in consecutive order that will be matched top to bottom by using the `else` keyword.

Here is an example of a `MyShow` typeclass, with instances for `String`, `Boolean`, and any other type.

```purescript
class MyShow a where
  myShow :: a -> String

instance showString :: MyShow String where
  myShow s = s

else instance showBoolean :: MyShow Boolean where
  myShow true = "true"
  myShow false = "false"

else instance showA :: MyShow a where
  myShow _ = "Invalid"

data MysteryItem = MysteryItem

main = do
  log $ myShow "hello" -- hello
  log $ myShow true -- true
  log $ myShow MysteryItem -- Invalid
```

## Multi-Parameter Type Classes

TODO. For now, see the section in [PureScript by Example](https://book.purescript.org/chapter6.html#multi-parameter-type-classes).

## Superclasses

Superclass implications can be indicated in a class declaration with a backwards fat arrow `<=`:

```purescript
class (Monad m) <= MonadFail m where
  fail :: forall a. String -> m a
```

This code example defines a `MonadFail` class with a `Monad` superclass: any type which defines an instance of `MonadFail` will be required to define an instance of `Monad` too.

Superclass instances will be used when searching for an instance of a subclass. For example, in the code below, the `Applicative` constraint introduced by the `pure` function can be discharged since `Applicative` is a superclass of `Monad`, which is in turn a superclass of `MonadFail`:

```purescript
assert :: forall m. (MonadFail m) => Boolean -> m Unit
assert true = pure unit
assert false = fail "Assertion failed"
```

## Orphan Instances

Type class instances which are defined outside of both the module which defined the class and the module which defined the type are called *orphan instances*. Some programming languages (including Haskell) allow orphan instances with a warning, but in PureScript, they are forbidden. Any attempt to define an orphan instance in PureScript will mean that your program does not pass type checking.

For example, the `Semigroup` type class is defined in the module `Data.Semigroup`, and the `Int` type is defined in the module `Prim`. If we attempt to define a `Semigroup Int` instance like this:

```purescript
module MyModule where

import Prelude

instance semigroupInt :: Semigroup Int where
  append = (+)
```

This will fail, because `semigroupInt` is an orphan instance. You can use a `newtype` to get around this:

```purescript
module MyModule where

import Prelude

newtype AddInt = AddInt Int

instance semigroupAddInt :: Semigroup AddInt where
  append (AddInt x) (AddInt y) = AddInt (x + y)
```

In fact, a type similar to this `AddInt` is provided in `Data.Monoid.Additive`, in the `monoid` package.

Orphan instances are banned because they can lead to incompatible duplicated instances for the same type and class. For example, suppose two separate modules define an orphan `Semigroup Int` instance, and one of them uses `+` for `append`, whereas the other uses `*`. Now suppose someone writes a third module which imports both of the first two, and that somewhere in that third module we have the expression `2 <> 3`, which calls for a `Semigroup Int` instance. The compiler now has two instances to choose from. What should it do? It could report an error, or it could arbitrarily pick one of the instances. Neither option is particularly appealing:

 * If it chooses to report an error, it means that any pair of modules which define the same orphan instance can never be used together.
 * If it arbitrarily picks one, we won't be able to determine whether `2 <> 3` will evaluate to 5 or 6. This can make it very difficult to ensure that your program will behave correctly!

Banning orphan instances also ensures global uniqueness of instances. Without global uniqueness, you risk operating on data with incompatible instances in different sections of code. For example, in Ord-based maps and sets, if it were possible to insert some values into a map using one `Ord` instance, and then try to retrieve them using a different `Ord` instance, you'd have keys disappear from your map. Another example is if you had a type class which defined serialization and deserialization operations, you'd be able to serialize something with one instance and then try to deserialize it with a different incompatible instance.

For multi-parameter type classes, the orphan instance check requires that the instance is either in the same module as the class, or the same module as at least one of the types occurring in the instance. (TODO: example)

## Functional Dependencies

Instances for type classes with multiple parameters generally only need a subset of the parameters to be concrete to match instances. Declarations on which parameters can determine others in instance heads are called Functional Dependencies. For example:

```purescript
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a

instance refl :: TypeEquals a a where
  to a = a
  from a = a
```

The `|` symbol marks the beginning of functional dependencies, which are separated by a comma if there are more than one. In this case, the first parameter determines the type of the second, and the second determines the type of the first.

Functional dependencies are especially useful with the various `Prim` typeclasses, such as `Prim.Row.Cons`: https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Cons

See also the section in [PureScript by Example](https://book.purescript.org/chapter6.html#functional-dependencies).

## Type Class Deriving

Some type class instances can be derived automatically by the PureScript compiler. To derive a type class instance, use the `derive instance` keywords:

```purescript
newtype Person = Person { name :: String, age :: Int }

derive instance eqPerson :: Eq Person
derive instance ordPerson :: Ord Person
```
Currently, the following type classes can be derived:

- [Data.Generic.Rep (class Generic)](https://pursuit.purescript.org/packages/purescript-generics-rep/6.0.0/docs/Data.Generic.Rep#t:Generic)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Data.Functor#t:Functor)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/3.0.0/docs/Data.Newtype#t:Newtype)

## Compiler-Solvable Type Classes

Some type classes can be automatically solved by the PureScript Compiler without requiring you place a PureScript statement, like `derive instance`, in your source code.

``` purescript
foo :: forall t. (Warn "Custom warning message") => t -> t
foo x = x
```

Automatically solved type classes are included in the [Prim](https://pursuit.purescript.org/builtins/docs/Prim) modules:

Symbol-related classes

- [`IsSymbol`](https://pursuit.purescript.org/packages/purescript-symbols/3.0.0/docs/Data.Symbol#t:IsSymbol)
- [`Append`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Append)
- [`Compare`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Compare)
- [`Cons`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Cons)

[Prim.Row](https://pursuit.purescript.org/builtins/docs/Prim.Row)

- [`Cons`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Cons)
- [`Union`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Union)
- [`Nub`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Nub)
- [`Lacks`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Lacks)

[Prim.RowList](https://pursuit.purescript.org/builtins/docs/Prim.RowList)

- [`RowToList`](https://pursuit.purescript.org/builtins/docs/Prim.RowList#t:RowToList)

Other classes

- [`Partial`](https://pursuit.purescript.org/builtins/docs/Prim#t:Partial)
- [`Fail`](https://pursuit.purescript.org/builtins/docs/Prim.TypeError#t:Fail)
- [`Warn`](https://pursuit.purescript.org/builtins/docs/Prim.TypeError#t:Warn)
