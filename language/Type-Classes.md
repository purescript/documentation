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

When type variables are present in the constraint being solved, the question of whether an instance in a chain matches is a little subtle. A type variable in the constraint is treated existentially—it represents some concrete type, but the constraint solver isn't allowed to assume that it is any particular type. This means that, when evaluating an instance as a solution for a particular constraint, there are three possible outcomes for the instance: it can match, it can fail to match, or it can be ambiguous. An ambiguous instance is one that could match the constraint only if one or more of the variables in the constraint happened to represent a particular type.

```purescript
class MyShow a where
  myShow :: a -> String

instance showStringTuple :: MyShow a => MyShow (Tuple String a) where
  myShow (Tuple s a) = s <> ": " <> myShow a

else instance showA :: MyShow a where
  myShow _ = "Invalid"

f :: forall l r. Tuple l r -> String
f = myShow -- error: no instance found
```

In this example, `f` needs an instance of `MyShow (Tuple l r)`. The `showStringTuple` is an ambiguous match for this constraint, because it would only match if `l` represented the type `String`. But `f` needs an implementation that will work for all `l`, so this can't be used as a full match.

Ambiguous instances are mostly treated the same as an instance that fails to match, with this exception. If an instance in an instance chain fails to match, the compiler will try the next instance in the chain. But if an instance in an instance chain is an ambiguous match, the compiler will not consider any more instances in that chain. In the above example, the compiler will report that no instance was found for `MyShow (Tuple l r)` in `f`, even though the `showA` instance could have been a match, because the ambiguous `showStringTuple` prevents `showA` from being considered.

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

The compiler can derive type class instances to spare you the tedium of writing boilerplate. There are a few ways to do this depending on the specific type and class being derived.

### Classes with built-in compiler support

Some classes have special built-in compiler support, and their instances can be derived from all types.

For example, if you you'd like to be able to remove duplicates from an array of an ADT using `nub`, you need an `Eq` and `Ord` instance. Rather than writing these manually, let the compiler do the work.

```purs
import Data.Array (nub)

data MyADT
  = Some
  | Arbitrary Int
  | Contents Number String

derive instance Eq MyADT
derive instance Ord MyADT

nub [Some, Arbitrary 1, Some, Some] == [Some, Arbitrary 1]
```

Currently, instances for the following classes can be derived by the compiler:
- Data.Generic.Rep (class Generic) 
- [Data.Bifoldable (class Bifoldable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Bifoldable#t:Bifoldable)
- [Data.Bifunctor (class Bifunctor)](https://pursuit.purescript.org/packages/purescript-bifunctors/docs/Data.Bifunctor#t:Bifunctor)
- [Data.Bitraversable (class Bitraversable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Bitraversable#t:Bitraversable)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Ord#t:Ord)
- [Data.Foldable (class Foldable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Foldable#t:Foldable)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#t:Functor)
- [Data.Functor.Contravariant (class Contravariant)](https://pursuit.purescript.org/packages/purescript-contravariant/docs/Data.Functor.Contravariant#t:Contravariant)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype)
- [Data.Profunctor (class Profunctor)](https://pursuit.purescript.org/packages/purescript-profunctor/docs/Data.Profunctor#t:Profunctor)
- [Data.Traversable (class Traversable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Traversable#t:Traversable)

#### `Functor`, `Foldable`, and `Traversable`

All three of these classes (and variations on them, like `Bifunctor`) can be derived for certain data types. The full table of supported classes is below:

<table>
<thead>
<tr><th colspan="2">monoparametric</th><th colspan="2">biparametric</th></tr>
</thead>
<tbody>
<tr><td><code>Functor</code></td><td><code>Contravariant</code></td><td><code>Bifunctor</code></td><td><code>Profunctor</code></td></tr>
<tr><td><code>Foldable</code></td><td>—</td><td><code>Bifoldable</code></td><td>—</td></tr>
<tr><td><code>Traversable</code></td><td>—</td><td><code>Bitraversable</code></td><td>—</td></tr></tbody>
</table>

Each row of this table defines a set of **compatible** classes; for the remainder of this section, a `Functor`-compatible class is any class in the top row of the table, etc.

The compiler can derive an instance of any of the above classes for your data type if the following requirements are met:

1. The data type must have at least one type parameter (or at least two type parameters, if a biparametric class is being derived).

   Occurrences of the final parameter (if deriving a monoparametric class) or final two parameters (if biparametric) in the constructors of your data type will be referred to below as **relevant variables**.

2. Each relevant variable must be in a legal position in the data type's definitions. Legal positions are:
   * Data constructor arguments
   * A field in a record type that itself appears in a legal position
   * An argument to a type constructor that itself appears in a legal position, *only if* one of the following holds:
       * If the constructed type is of the form `f a b ... x y`, and `f a b ... x` has an instance of a monoparametric class compatible with the class being derived, then `y` is a legal position.
       * If the constructed type is of the form `f a b ... x y z`, and `f a b ... x` has an instance of a biparametric class compatible with the class being derived, then `y` and `z` are legal positions.

     (For the highly detail-oriented: if both conditions are met, and the penultimate argument doesn't contain any relevant variables, the monoparametric instance will be preferred. This shouldn't ever cause problems if all instances are well-behaved; for example, `rmap` and `map` are expected to be equivalent when both are available.)

3. If deriving a `Functor`-compatible class, each relevant variable must be used with its expected **variance** (covariant or contravariant). When deriving `Functor` or `Bifunctor`, all variables are expected to be covariant. When deriving `Contravariant`, the variable is expected to be contravariant. When deriving `Profunctor`, the first variable is expected to be contravariant and the second covariant.

   A type variable is used covariantly if it occurs exclusively in positive positions in the data type's definition, and contravariantly if it occurs exclusively in negative positions. Positive and negative positions are defined similarly to legal positions:
   * Data constructor arguments are positive positions.
   * A field in a record type in a positive (resp. negative) position is itself a positive (resp. negative) position.
   * An argument to a type constructor may keep or invert the sign of the (constructed) type's position, depending on the variance of the `Functor`-compatible instance chosen for that type when determining if the argument is a legal position. An argument corresponding to a covariant (resp. contravariant) parameter keeps (resp. inverts) the sign.

     (Another remark for the highly detail-oriented: in the rare case that both `Functor` and `Contravariant`, or both `Bifunctor` and `Profunctor`, instances are available for a type constructor such that a position could be either positive or negative, the compiler chooses `Functor` or `Bifunctor`. This case is rare because only phantom parameters can be both covariant and contravariant.)

(In most cases, the lack of orphan instances in PureScript means that the compiler will be able to automatically find any instances that might be relevant for the above requirements if they exist. However, a small number of `Functor`-compatible instances are defined in packages that may not already be included in your project if you are attempting to derive a different `Functor`-compatible class. In that case, if you are expecting to use the `Bifunctor`, `Contravariant`, or `Profunctor` instances for any of `Const`, `Either`, `Function`, or `Tuple`, you will have to add `purescript-bifunctors`, `purescript-contravariant`, or `purescript-profunctor` as appropriate to the dependencies of your project before the compiler will proceed with deriving the class. This list of exceptional instances may change as the PureScript standard libraries evolve.)

The examples below indicate valid usages (via `✓`) and invalid usages (via `⨯`) of a type parameter `a` used in various places below.
If the invalid usages are removed from the data constructors, the compiler can derive `Functor`, `Foldable`, and `Traversable` instances for it.

```purs
data X f a
  = X0 Int
  --         - since no `a` appears in this data constructor
  --           it doesn't break any of the rules above
  | X1 a a a
  --   ✓ ✓ ✓ - data constructor arguments are legal positions for `a`
  | X2 (f a)
  --      ✓ - because the `a` is the rightmost argument to `f`
  --          `f` will be required to be a `Functor`, etc.
  | X3 (Tuple a Int)
  --          ✓ - `Tuple` needs to be a `Bifunctor`, etc. (which it is, if `purescript-bifunctors` is in your project's dependencies)
  | X4 (Tuple a a)
  --          ✓ ✓
  | X5 { foo :: a, bar :: f a, baz :: Tuple Int a }
  --            ✓           ✓                   ✓ - records are supported
  | X6 { one :: { two :: { three :: a } } }
  --                                ✓ - even nested ones
  | X7 (Foo a Int Int)
  --        ⨯ - this `a` is the third-to-last argument and can't be legal no matter what `Foo` implements
  | X8 (Variant (left :: a, right :: Int))
  --                     ⨯ - row types aren't included in the definition of legal positions
```

For `Foldable` and `Traversable` (and compatible classes), records are folded and traversed in the alphabetical ordering of their labels, not their definition order.
For example, a record defined like
```purs
type Foo a =
  { m :: a
  , f :: a
  , c :: a
  , g :: a
  }
```

will be traversed in a `cfgm` order, not the `mfcg` order.

Given a data type like the following...

```purs
data M f a
  = M0
  | M1 a (Array a)
  | M2 Int
  | M3 (f a)
  | M4 { a :: a, x :: Array a, fa :: f a
       , ignore :: Int, no :: Array Int, nope :: f Int
       , nested :: { anotherOne :: a }
       }

derive instance Foldable f => Foldable (M f)
```

Something like the following will be generated:
```purs
instance Foldable f => Foldable (M f) where
  foldl f z = case _ of
    M0 -> z
    M1 a arr -> foldl f (f z a) arr
    M2 _ -> z
    M3 fa -> foldl f z fa
    M4 rec ->
      foldl f (foldl f (foldl f (f z rec.a) rec.fa) rec.nested.anotherOne) rec.x
  foldr f z = case _ of
    M0 -> z
    M1 a arr -> f a (foldr f z arr)
    M2 _ -> z
    M3 fa -> foldr f z fa
    M4 rec ->
      f (foldr f (foldr f (f z rec.x) rec.nested.anotherOne) rec.fa) rec.a
  foldMap f = case _ of
    M0 -> mempty
    M1 a arr -> f a <> foldMap f arr
    M2 _ -> mempty
    M3 fa -> foldMap f fa
    M4 rec -> f rec.a <> foldMap f rec.fa <> foldMap f rec.nested.anotherOne <> foldMap rec.x
```

Finally, note that superclasses are not automatically derived; if you derive `Traversable`, you will also need explicit or derived instances of `Functor` and `Foldable`.

### Derive from `newtype`

If you would like your newtype to defer to the instance that the underlying type uses for a given class, then you can use newtype deriving via the `derive newtype` keywords.

For example, let's say you want to add two `Score` values using the `Semiring` instance of the wrapped `Int`.

```purs
newtype Score = Score Int

derive newtype instance semiringScore :: Semiring Score

tenPoints :: Score
tenPoints = (Score 4) + (Score 6)
```

That `derive` line replaced all this code:

```purs
-- No need to write this
instance semiringScore :: Semiring Score where
  zero = Score 0
  add (Score a) (Score b) = Score (a + b)
  mul (Score a) (Score b) = Score (a * b)
  one = Score 1
```

Note that we can use either of these options to derive an `Eq` instance for a `newtype`, since `Eq` has built-in compiler support. They are equivalent in this case.

```purs
derive instance Eq Score
derive newtype instance Eq Score
```

### Deriving from `Generic`

The compiler's built-in support for `Generic` unlocks convenient deriving for many other classes not listed above. See the [deriving guide](../guides/Type-Class-Deriving.md#deriving-from-generic) for more information.

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
- [`Coercible`](https://pursuit.purescript.org/builtins/docs/Prim.Coerce#t:Coercible)
