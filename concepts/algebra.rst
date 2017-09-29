Algebra
=======

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+-------------------+---------------------------------------------------------+
| Dual              | An opposite and complementary concept.                  |
+-------------------+---------------------------------------------------------+
| Cartesian product | For sets A and B, the Cartesian product A × B is the    |
|                   | set of all ordered pairs (a, b) where a ∈ A and b ∈ B.  |
+-------------------+---------------------------------------------------------+
| Relation          | A mapping from elements of one set to another           |
+-------------------+---------------------------------------------------------+
| Function          | A relation where one source maps to only one target     |
+-------------------+---------------------------------------------------------+
| Partial function  | A function that is not defined for some values          |
|                   | of its parameters                                       |
+-------------------+---------------------------------------------------------+
| Total function    | A function which is defined for all values              |
|                   | of its parameters.                                      |
+-------------------+---------------------------------------------------------+
| Homomorphism      | A homomorphism is a structure-preserving map between    |
|                   | two algebraic structures of the same type.              |
|                   | Literally meaning, having same shape.                   |
+-------------------+---------------------------------------------------------+

Classes
-------

+-------------------+---------------------------------------------------------+
| Class             |                                                         |
+-------------------+---------------------------------------------------------+
| Proper class      |                                                         |
+-------------------+---------------------------------------------------------+

Sets
----

+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| Structure                                                | Definition                                               | Example           |
+==========================================================+==========================================================+===================+
| `Set <https://en.wikipedia.org/wiki/Set_(mathematics)>`_ | A collection of distinct objects.                        | {1, 2, 3, 4, ...} |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+

In Haskell, data types correspond to the sets in mathematics. For example the
type `Int` represents the following set of values::

  {..., -3, -2, -1, 0, 1, 2, 3, ...}

Composite Sets
--------------

Larger composite sets can be created by combining primitive sets. Two very
useful combining operations are `cartesian product` or simply `product`, and
`coproduct` or `sum`.

Algebraic data types (ADT) in Haskell are defined as products or sums of
primitive or basic data types.

Cartesian Product
~~~~~~~~~~~~~~~~~

Given two sets `A` and `B`, `A × B` is the cartesian product. It represents all
tuples `(a,b)` where `a` is from set `A` and `b` is from set `B`. If `A` has
`m` elements and `B` has `n` elements then `A × B` will have `m × n`  elements.

In Haskell a product of two types (a type represents a set) can be represented
as a two tuple, the following two forms are equivalent:

+-------------------------------+---------------------------------------------+
| ``data (a, b) = (a, b)``      | ``data Product = Product Int Int``          |
+-------------------------------+---------------------------------------------+

A cartesian product `Int × Int` i.e. `(Int, Int)` type can have values like::

  (0,0)
  (0,1)
  (0,-1)
  (1,0)
  (1,-1)
  ...

Coproduct or Sum
~~~~~~~~~~~~~~~~

A coproduct or sum is the disjoint union of two sets. An element of the
disjoint union of `a` and `b` is either an element of `a` or an element of `b`.
If the two sets overlap, the disjoint union contains two copies of the
overlapping elements.  Coproduct is a concept from category theory and is a
dual of product. It is also called a sum.

The Haskell data type `Either` is a coproduct of data types `a` and `b`::

  data Either a b = Left a | Right b

You can think of an element of a disjoint union as being tagged with an
identifier that specifies its originating set. Therefore, it is also called a
`tagged union`, `Left` and `Right` being the tags.

A coproduct represents a choice or disjunction in contrast to product which
represents a combination or conjunction.

Mappings Between Sets
---------------------

(Binary) Relations
~~~~~~~~~~~~~~~~~~

A relation `maps` elements of one set to another set.  A `binary relation`
between two sets is represented by a set of pairs of related elements of the
two sets.  In other words, a relation from set `X` to set `Y` is a subset of
their Cartesian product `X × Y`. `X` is called the `domain` of the relation and
`Y` is called `codomain` or `range`. Relations are also known as
`correspondences`.

An example of a relation is `greater than` between two sets of natural numbers.
This relation maps each element in one set to every element in the other set.

`Functions <https://en.wikipedia.org/wiki/Function_(mathematics)>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function is a binary relation where the first value of every tuple is unique
through the set. That is, one element in the domain maps to at most one element
in the range.

A function `f` that maps values from a set `X` to another set `Y` is denoted as
`f: X → Y`.  `X` is called the domain and `Y` the codomain of `f`.

.. |invalid| image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/algebra/invalid.png
.. |total| image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/algebra/total.png
.. |partial| image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/algebra/partial.png

+------------+------------------------------------------+---------------------------+
| Type       | Property                                 | Examples                  |
+============+==========================================+===========================+
| Valid      | f(X) maps to only one element in Y       | |invalid|                 |
+------------+------------------------------------------+-------------+-------------+
| Total      | f(X) is defined for all elements in X    | |total|     | |partial|   |
+------------+------------------------------------------+-------------+-------------+
| Injective  | one-to-one i.e. f(x1) /= f(x2)           |             |             |
+------------+------------------------------------------+-------------+-------------+
| Surjective | onto i.e. each element in Y is mapped to |             |             |
+------------+------------------------------------------+-------------+-------------+
| bijective  | Injective & surjective i.e.              |             |             |
|            | one-to-one from whole X to whole Y       |             |             |
+------------+------------------------------------------+-------------+-------------+

Relations are a generalization of functions. For example, `greater than` is a
relation but not a valid function because it can map one element in a domain to
more than one element in codomain.

Types of Functions
------------------

Unary Operation
~~~~~~~~~~~~~~~

A unary operation transforms a type into another type or in other words it maps
the values from one set (type) to values in another set (type).

+-------------------------------+---------------------------------------------+
| Math Notation                 | Haskell Type                                |
+-------------------------------+---------------------------------------------+
|  f: X → Y                     | f :: x -> y                                 |
+-------------------------------+---------------------------------------------+

Binary Operation
~~~~~~~~~~~~~~~~

A binary operation combines two objects of potentially different types into a
third object of potentially different type. In other words, it maps the
cartesian product of two sets (types) to a target set (type). In Haskell a
binary functions are implemented by currying that is applying one argument at a
time.

+-------------------------------+---------------------------------------------+
| Math Notation                 | Haskell Type                                |
+-------------------------------+---------------------------------------------+
| ::                            | ::                                          |
|                               |                                             |
|  f: X → Y → Z                 |  f :: (x, y) -> z                           |
|                               |  f :: x -> y -> z                           |
|                               |  f :: x -> (y -> z)                         |
+-------------------------------+---------------------------------------------+

N-ary Operations
~~~~~~~~~~~~~~~~

An N-ary operation combines `n` objects of potentiall different types into one
object of potentially different type or in other words it maps the product of n
sets (types) into a target set. In Haskell, n-ary operations are implemented by
currying i.e. applying one argument at a time.

Algebraic Structures
--------------------

An algebraic structure is a set (called `carrier set` or underlying set) with
one or more operations defined on it that satisfies a list of axioms. Some
examples of algebraic structures of programming importance are semigroups and
monoids.

In Haskell, a set is represented by a `type` and operations are `functions` on
the type.

Algebraic Composition
---------------------

Composing objects (Magma)
~~~~~~~~~~~~~~~~~~~~~~~~~

`Magma` is a family of algebraic structures that allow us to combine multiple
objects in a set using binary functions. The most basic structure, magma, is
incrementally specialized to derive more restrictive and sophisticated
structures.

+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| Structure                                                | Definition                                               | Example                             |
+==========================================================+==========================================================+=====================================+
| `Magma <https://en.wikipedia.org/wiki/Magma_(algebra)>`_ | A set with a single binary operation (closed, M × M → M) | {1, 2, 3, ...}                      |
|                                                          |                                                          |                                     |
|                                                          |                                                          | ``x1 + x2 = x3``                    |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Semigroup <https://en.wikipedia.org/wiki/Semigroup>`_   | A magma with a binary operation that is associative      | ``(x1 + x2) + x3 = x1 + (x2 + x3)`` |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Monoid <https://en.wikipedia.org/wiki/Monoid>`_         | A semigroup with an identity element                     | {0, 1, 2, 3, ...}                   |
|                                                          |                                                          |                                     |
|                                                          |                                                          | ``x + 0 = x = 0 + x``               |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Group                                                   | A monoid with an invertible operation                    |                                     |
| <https://en.wikipedia.org/wiki/Group_(mathematics)>`_    |                                                          |                                     |
|                                                          |                                                          | ``x * y = id = y * x``              |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+

Haskell Typeclasses
~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Algebraic structures to compose multiple objects using a binary function    |
+-------------------------------------+---------------------------------------+
| Combine                             | Fold                                  |
+=====================================+=======================================+
| Semigroup                           | Monoid                                |
+-------------------------------------+---------------------------------------+

Monoid
~~~~~~

A `Monoid` is the most commonly used structure because it is simple enough to
not be too restrictive and sophisticated enough to be quite useful.

A simple example of a Monoid in Haskell is a two tuple e.g. (a, b). For example

* `(Int, Int)` is a monoid under addition (+) with 0 as the identity.
* `(Int, Int)` is a monoid under multiplication (*) with 1 as the identity.

Global and local identities::

  ∃x∀y x.y=y.x=y
  ∀y∃x x.y=y.x=y

Summary
-------

+-------------------+----------------+-------------------------------+--------------------+
| Algebraic Concept | Notation       | Corresponding Haskell Concept | Notation           |
+===================+================+===============================+====================+
| Set               | `X`            | Type                          | `x`                |
+-------------------+----------------+-------------------------------+--------------------+
| Cartesian Product | `X × Y`        | Tuple                         | `(x, y)`           |
+-------------------+----------------+-------------------------------+--------------------+
| Coproduct or Sum  | `X + Y`        | Either                        | `Left x` or        |
|                   |                |                               | `Right y`          |
+-------------------+----------------+-------------------------------+--------------------+
| Unary operation   | `f: X → Y`     | Single argument function      | `f :: x -> y`      |
+-------------------+----------------+-------------------------------+--------------------+
| Binary Operation  | `f: X × Y → X` | Uncurried form                | `f :: (x, y) -> x` |
|                   |                +-------------------------------+--------------------+
|                   |                | Curried form                  | `f :: x -> y -> x` |
+-------------------+----------------+-------------------------------+--------------------+
