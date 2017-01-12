Terminology
-----------

+-------------------+---------------------------------------------------------+
| Class             |                                                         |
+-------------------+---------------------------------------------------------+
| Proper class      |                                                         |
+-------------------+---------------------------------------------------------+
| Cartesian product | For sets A and B, the Cartesian product A × B is the    |
|                   | set of all ordered pairs (a, b) where a ∈ A and b ∈ B.  |
+-------------------+---------------------------------------------------------+
| Homomorphism      | A homomorphism is a structure-preserving map between    |
|                   | two algebraic structures of the same type.              |
|                   | Literally meaning same shape.                           |
+-------------------+---------------------------------------------------------+
| Partial function  | A function that is not defined for some values          |
|                   | of its parameters                                       |
+-------------------+---------------------------------------------------------+
| Total function    | A function which is defined for all values              |
|                   | of its parameters.                                      |
+-------------------+---------------------------------------------------------+

Sets
----

+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| Structure                                                | Definition                                               | Example           |
+==========================================================+==========================================================+===================+
| `Set <https://en.wikipedia.org/wiki/Set_(mathematics)>`_ | A collection of distinct objects.                        | {1, 2, 3, 4, ...} |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+

A data type in Haskell represents a set. For example an `Int` type represents
{..., -3, -2, -1, 0, 1, 2, 3, ...}

Cartesian Product
~~~~~~~~~~~~~~~~~

Given two sets X and Y, X × Y is the cartesian product. It represents all
tuples (a,b) where a is from set X and b is from set Y. If X has m elements and
Y has n elements then X × Y will have m × n  elements.

In Haskell a product of two sets can be represented as::

  data (a, b) = (a, b)

A cartesian product `Int × Int` i.e. `(Int, Int)` type can have values like::

  (0,0)
  (0,1)
  (0,-1)
  (1,0)
  (1,-1)
  ...

(Binary) Relations
------------------

A relation "maps" elements of one set to another set.  A binary relation
between two sets is represented by a set of pairs of related elements of the
two sets.  In other words, a relation from set `X` to set `Y` is a subset of
their Cartesian product `X × Y`. `X` is called the domain of the relation and
`Y` is called codomain or range. Relations are also called `correspondences`.

Relations are a generalization of functions. For example, `greater than` is a
relation but not a valid function because it can map one element in a domain to
more than one element in codomain.

`Functions <https://en.wikipedia.org/wiki/Function_(mathematics)>`_
-------------------------------------------------------------------

A function is a binary relation where the first value of every tuple is unique
through the set. That is one element in the domain maps to at most one element
in the range.

A function `f` that maps values from a set `X` to another set `Y` is denoted as
`f: X → Y`.  `X` is called the domain and `Y` the codomain of `f`.

+------------+------------------------------------------+---------+-----------------+
| Type       | Property                                 | Example | Counter Example |
+============+==========================================+=========+=================+
| Valid      | f(X) maps to only one element in Y       |         |                 |
+------------+------------------------------------------+---------+-----------------+
| Total      | f(X) is defined for all elements in X    |         |                 |
+------------+------------------------------------------+---------+-----------------+
| Injective  | one-to-one i.e. f(x1) /= f(x2)           |         |                 |
+------------+------------------------------------------+---------+-----------------+
| Surjective | onto i.e. each element in Y is mapped to |         |                 |
+------------+------------------------------------------+---------+-----------------+
| bijective  | Injective & surjective i.e.              |         |                 |
|            | one-to-one from whole X to whole Y       |         |                 |
+------------+------------------------------------------+---------+-----------------+

Unary Operation - Transform
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A unary operation is a pure transformation which transforms its argument to
another object::

  f: X → Y

In Haskell::

  f :: x -> y

Binary Operation - Combine
~~~~~~~~~~~~~~~~~~~~~~~~~~

A function on two objects. A binary operation combines two objects into a
single objects in some way.

Binary operation is the most basic combining operations. All other functions
with multiple arguments can be represented in terms of binary operations.

A function from cartesian product `X × Y` to `X` is mathematically denoted as `f: X × Y → X`.

In Haskell the function can be represented as::

  f :: (x, y) -> x -- tuple form
  f :: x -> y -> x -- curried form
  f :: x -> (y -> x) -- A chain of two functions

Where type `x` represents set `X` and type `y` represents set `Y`.

Fixpoint
--------

::

  f(c) = c

Summary
-------

+-------------------+----------------+-------------------------------+--------------------+
| Algebra           | Notation       | Haskell                       | Notation           |
+===================+================+===============================+====================+
| Set               | `X`            | Type                          | `x`                |
+-------------------+----------------+-------------------------------+--------------------+
| Cartesian Product | `X × Y`        | Tuple                         | `(x, y)`           |
+-------------------+----------------+-------------------------------+--------------------+
| Unary operation   | `f: X → Y`     | Single argument function      | `f :: x -> y`      |
+-------------------+----------------+-------------------------------+--------------------+
| Binary Operation  | `f: X × Y → X` | Uncurried form                | `f :: (x, y) -> x` |
|                   |                +-------------------------------+--------------------+
|                   |                | Curried form                  | `f :: x -> y -> x` |
+-------------------+----------------+-------------------------------+--------------------+
