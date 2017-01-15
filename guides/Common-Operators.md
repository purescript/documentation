# Common Operators

Or, "what is the PureScript equivalent of \<X operator\> in JS?"

Although the PureScript language defines no built-in operators, plenty are defined in the `Prelude` and other core libraries. This guide gives you a overview of these operators and the corresponding operators in JavaScript.

## JavaScript's unary operators

All PureScript operators are binary, with one exception: there is syntax sugar for unary negation, whereby in PureScript code, `-x` is desugared to `negate x`, which uses whichever `negate` is in scope.

This means that the PureScript analogues to unary JavaScript operators (other than `-`) are not operators, but normal functions which must be applied using the standard prefix-style function application, so e.g. `!x` in JS must be written as `not x` in PureScript.

| JS Operator | PureScript Function | Defined in            | Meaning          |
|-------------|---------------------|-----------------------|------------------|
| `-`         | `negate`            | `Data.Ring`           | Numeric negation |
| `!`         | `not`               | `Data.HeytingAlgebra` | Boolean negation |
| `~`         | `complement`        | `Data.Int.Bits`       | Bitwise negation |

## JavaScript's binary operators

| JS Operator | PureScript Function | Defined in            | Meaning                                  |
|-------------|---------------------|-----------------------|------------------------------------------|
| `+`         | `+`                 | `Data.Semiring`       | Numeric addition                         |
| `+`         | `<>`                | `Data.Semigroup`      | String concatenation                     |
| `-`         | `-`                 | `Data.Ring`           | Numeric subtraction                      |
| `*`         | `*`                 | `Data.Semiring`       | Numeric multiplication                   |
| `/`         | `/`                 | `Data.EuclideanRing`  | Numeric division                         |
| `%`         | `%` (see note)      | `Math`                | Remainder, the same as JS' `%` operator. |
| `%`         | `mod` (see note)    | `Data.EuclideanRing`  | Also remainder (see note)                |
| `==`        | `==`                | `Data.Eq`             | Equality check                           |
| `!=`        | `/=`                | `Data.Eq`             | Inequality check                         |
| `<`         | `<`                 | `Data.Ord`            | Less than                                |
| `<=`        | `<=`                | `Data.Ord`            | Less than or equal                       |
| `>`         | `>`                 | `Data.Ord`            | Greater than                             |
| `>=`        | `>=`                | `Data.Ord`            | Greater than or equal                    |
| `&&`        | `&&`                | `Data.HeytingAlgebra` | Boolean AND                              |
| `||`        | `||`                | `Data.HeytingAlgebra` | Boolean OR                               |
| `&`         | `.&.`               | `Data.Int.Bits`       | Bitwise AND                              |
| `|`         | `.|.`               | `Data.Int.Bits`       | Bitwise OR                               |
| `^`         | `.^.`               | `Data.Int.Bits`       | Bitwise XOR                              |
| `<<`        | `shl`               | `Data.Int.Bits`       | Shift Left                               |
| `>>`        | `shr`               | `Data.Int.Bits`       | Shift Right                              |
| `>>>`       | `zshr`              | `Data.Int.Bits`       | Zero-fill Shift Right                    |

Most of these functions and operators are re-exported by the `Prelude`, so to have access to them in your code, you should normally just write `import Prelude`.

Additionally, many of these functions and operators are defined in type classes and work with lots of different types. For example, `+` and `*` work with not only `Int` and `Number`, but also any instance of the `Semiring` type class defined in the `Prelude`.

### A note on remainder/modulus (`%`)

Normally, when we ask for the remainder of one number after dividing by another, we are talking about integers. Clearly the remainder of 10 after dividing by 3 is 1, and indeed in JS, `10 % 3 == 1`. Likewise, in PureScript, `10 ``mod`` 3 == 1`.

It's a bit harder to say what a 'remainder' operation should mean if we are trying to extend it to real (non-integral) numbers, though. Real numbers can be divided into one another with nothing 'left over', i.e. in some sense, the remainder of dividing one number by another is always 0.

Since JS just has one number type, it has to come up with a sensible way of handling cases where one or both of its arguments are non-integral. To address this, it uses a behaviour where it tries to find the largest *integer* multiple of the second argument which is smaller than the first argument, and then returns the difference between these. For example, in JS, `10.5 % 3 == 1.5`.

If this is the behaviour you want in PureScript, you should use `%` from the `Math` module in `purescript-math`. Its type is `Number -> Number -> Number` and it simply delegates to the `%` operator in JS.

However, PureScript's `Prelude` aims to provide a stronger theoretical foundation for common operators such as this one, which is why the `Prelude` exports a slightly different function, `mod :: forall a. EuclideanRing a => a -> a -> a`. For integers, `mod` works how you expect it to: `10 ``mod`` 3 == 1`, just as before. However, for Numbers, `mod` always returns 0. This may be surprising; however, the reason it works this way is that it is based upon a mathematical structure called a *Euclidean Ring*, whose definition requires this behaviour. For more info, see the [EuclideanRing documentation](https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.EuclideanRing#t:EuclideanRing).