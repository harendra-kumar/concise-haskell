Transform One
-------------

A `transform` operation is an abstract notion, and the simplest possible
operation on a single value.  It transforms or maps a value in one domain to a
corresponding value in another domain.

An example of such the operation is `colorCode` that maps a color from the
`Color` domain to a number in `Integer` domain::

  colorCode :: Color -> Integer

  colorCode Red   = 1
  colorCode Green = 2
  colorCode Blue  = 3

Another example is the operation `succ` that returns the successor of a number.
It maps a number from the `Integer` domain to its successor in the same domain::

  succ :: Integer -> Integer

  succ 1 = 2
  succ 2 = 3
  ...

Think about ``colorCode`` and ``succ`` as abstract notions representing a
mapping or transformation from one type of value to another type. We call them
`unary functions` or `unary operations` in mathematical parlance.

General Unary Functions
~~~~~~~~~~~~~~~~~~~~~~~

In Haskell, the domains are Haskell types. A general unary function would
transform a type `a` into another type `b`.  We can represent such a function
by the following type signature::

  f :: a -> b
  f :: a -> a

What can we do with them?
~~~~~~~~~~~~~~~~~~~~~~~~~

It is interesting to see how we can use a function without knowing the specific
type of values that it works on.

In what ways can a unary operation be useful? What can we do with a single
value? We can apply a unary operation to transform it.

Single function, single value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`f :: a -> b` can just transform a value in another type and that's it.
`f :: a -> a` is more interesting. Since the output is of the same type we can
use the same function on output again. This is called iteration. If we keep on
feeding the output to the same function we will either converge or not. We
can only converge if f x = x for some x. This is called the fixed point of the
function.

Set of functions, single value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can apply another unary operation on the resulting value to transform it
again and so on. For example::

  colorCode Green => 2
  succ 2          => 3

Single function, Set of values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If we have a collection of values of a given type we can think of transforming
the whole collection into a new collection of a different type. For example::

  colorCode <$> [Red, Green, Blue] => [1, 2, 3]
  succ      <$> [1, 2, 3]          => [2, 3, 4]

Combine Two
-----------

A combine operation is an abstract notion, that takes two values from two
domains and combines them in some way to produce a value in a third domain. In
other words, it transforms a combination of two values to a third value in
another domain.

A simple example of a binary operation is an addition operation which adds two
numbers, both in the number domain, and produces a third number which is called
the sum of the two numbers, also in the number domain::

  add :: Integer -> Integer -> Integer

  add 1 1 = 2
  add 1 2 = 3
  ...
  add 2 1 = 3
  add 2 2 = 4
  ...

Another example, is a tuple operation::

  tuple :: String -> Integer -> (String, Integer)

  tuple "a" 1 = ("a",1)
  tuple "b" 2 = ("b",2)
  ...

Think about ``add`` and ``tuple`` as abstract notions combining two types of
values into a value of third type. We call them `binary functions` or `binary
operations` in mathematical parlance.

In Haskell, a general binary function would transform a type `a` and a type `b`
into third type `c`.  We can represent such a function by the following type
signature::

  f :: a -> b -> c

Any two types or all the three types could be different or the same::

  f :: a -> a -> a
  f :: a -> a -> b
  f :: a -> b -> a
  f :: a -> b -> b

How is a binary operation useful?

Combining using binary functions
--------------------------------

In Haskell we usually build bigger structures using binary operations. For
example the canonical product and sum types are (a,b) and Either a b
respectively. We can build bigger product and sum types by just combining them
using these two types! For example, functions are composed in a binary fashion
using currying. async package combines fork in a binary fashion to implement
parallel applicative or race alternative.
