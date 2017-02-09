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

Haskell algebraic data types are defined as products or sums of primitive or
basic data types.

Cartesian Product
~~~~~~~~~~~~~~~~~

Given two sets `A` and `B`, `A × B` is the cartesian product. It represents all
tuples `(a,b)` where `a` is from set `A` and `b` is from set `B`. If `A` has
`m` elements and `B` has `n` elements then `A × B` will have `m × n`  elements.

In Haskell a product of two sets (or types) can be represented as a two tuple::

  data (a, b) = (a, b)

Or like this, a product of two `Int` values can be represented by a new
`Product` type::

  data Product = Product Int Int

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
If the two sets overlap, the disjoint union contains two copies of the common
part. You can think of an element of a disjoint union as being tagged with an
identifier that specifies its origin.

A coproduct represents a choice. The Haskell data type `Either` is a coproduct
of data types `a` and `b`::

  data Either a b = Left a | Right b

It is also called a `tagged union`, `Left` and `Right` being the tags.

Coproduct is a concept from category theory and is a dual of product. It is
also called a sum.

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

Unary Operation - Transform
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A unary operation is a pure transformation which transforms its argument to
another object::

  f: X → Y

In Haskell::

  f :: x -> y

Binary Operation - Combine & Transform
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function on two objects. A binary operation combines two objects into a
single objects in some way.  A binary operation is the most basic combining
operations. All other functions with multiple arguments can be represented in
terms of binary operations.

A function from the cartesian product `X × Y` to `X` is mathematically denoted
as `f: X × Y → X`.

In Haskell, assuming type `x` represents set `X` and type `y` represents set
`Y`, this function can be represented by the following equivalent forms::

  f :: (x, y) -> x   -- tuple or uncurried form
  f :: x -> y -> x   -- curried form
  f :: x -> (y -> x) -- two chained functions


Algebraic Structure
-------------------

An algebraic structure is a set (called `carrier set` or underlying set) with
one or more operations defined on it that satisfies a list of axioms. Some
examples of algebraic structures of programming importance are semigroups and
monoids.

In Haskell, a set is represented by a `type` and operations are `functions` on
the type.

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
