# `InvalidInstanceHead` Error

## Example

As of 0.11.6, the `InvalidInstanceHead` error has been added to the
purescript compiler. The error can be produced using the following:

```purescript
module InvalidInstanceHead where

class ClassWithInvalidInstanceHead (x :: # Type)

instance FailingExample :: ClassWithInvalidInstanceHead ()
...
```

## Cause

Normally, when you see this error, it's because you're trying to write an
`instance` of a given `class` that pattern matches on some **row type**. The
issue here is that there isn't really a sensible way to match on row. There
are, however, ways to get around this limitation.

## Fix

With the arrival of [`purescript-typelevel-prelude`](https://github.com/purescript/purescript-typelevel-prelude)
2.3.0 came the introduction of `RowToList` and its related classes. The
basic idea is that you can convert a row type to a type-level cons list of its
entries. For example:

```purescript
--        ROW              LIST

RowToList ()               Nil
RowToList (a :: A)         (Cons "a" A Nil)
RowToList (a :: A, b :: B) (Cons "a" A (Cons "b" B Nil))
```

The `RowToList` constraint allows us to derive, from a row type, the `RowList`
that complements it. The keys of the `RowList` are alphabetically ordered, which
allows us to manipulate row types in a deterministic way. If your instance heads
pattern match on `RowList` instead of `# Type`, this error goes away.

## Worked Example

Let's imagine a class that we use to assert that a record is homogenous. Or, in
simpler terms, all the fields in the record have the same type.

```purescript
class IsHomogenousRowType (rowType :: # Type)
```

Now, as we've seen earlier, we can't just pattern-match on this in its current
state; we need to convert it to a `RowList`. How do we do that?

```purescript
instance isHomogenousImpl
  :: ( RowToList rowType rowList
     , IsHomogenousRowList rowList a
     )
  => IsHomogenousRecord rowType
```

Here, we say that, "if you can find me a `RowList` representation for my record
that satisfies `IsHomogenousRow`, then this record is good to go". Notice how
we're "pattern matching" on the left side of `RowToList` - we don't know what
`la` is, but we trust the compiler to iron out that little detail for us.

Now we implement `IsHomogenousRowList`. The type parameter `list` is the `RowList`
we want to check for homogeneously-typed values, which all have type `a`.

```purescript
class IsHomogenousRowList (list :: RowList) a | list -> a
```

Because we're now dealing with a `RowList`, we can pattern match and avoid those
pesky errors about instance heads. Our functional dependency here says that, "if
you can give me the type of `list`, I can always give you the type of `a`". The
first example of this is a special case:

```purescript
instance isHomogenousRowListImplEmpty
  :: IsHomogenousRowList Nil Unit
```

We just need to pick a type to use when we're dealing with an empty row; unit is
inoffensive enough, so we'll go with that. Next up, what happens if we only have
one entry?

```purescript
instance isHomogenousRowListImplNonEmpty
  :: IsHomogenousRowList (Cons k a Nil) a
```

When we have a singleton object, the instance is trivial; the record is always
homogenous. All we're doing here is saying that the row is homogenous if we can
find a type that matches the type of the single key. Easy!

Finally, we have to deal with the other cases. This can be generalised:

```purescript
instance isHomogenousRowListImplReallyNonEmpty
  :: IsHomogenousRowList la a
  => IsHomogenousRowList (Cons k a la) a
```

Here, we match the row as `Cons k a la`. If `la` is a homogenous row of type `a`
and `a` is the type of our row head, then we're all good! Success! Notice we end
up with two base cases here: an empty record and a singleton. Why? Well, if we
had only the empty one, it would only work for records where all fields were of
type `Unit`. Think about why this is.

At this point, everything would look as though it should work, right? We can
write a little helper function to that class stuff so we can use it in pratice:

```purescript
validateHomogeneity
  :: forall ra
   . IsHomogenousRecord ra
  => Record ra
  -> Record ra
validateHomogeneity = id
```

Now, if we call `validateHomogeneity` on our record, it will be checked at
*compile time*. With `RowToList`, we recover all the ability to pattern match
on row types that we would otherwise have lost.

If you'd like to play around with the examples above, you can use TryPureScript
to play with it in an [interactive
Gist](http://try.purescript.org/?gist=cf4a032ae0741b7c21994c93cfb3c633).
