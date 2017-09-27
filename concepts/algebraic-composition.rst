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
| `Semigroup <https://en.wikipedia.org/wiki/Semigroup>`_   | A magma where binary operation is associative            | ``(x1 + x2) + x3 = x1 + (x2 + x3)`` |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Monoid <https://en.wikipedia.org/wiki/Monoid>`_         | A semigroup with an identity element                     | {0, 1, 2, 3, ...}                   |
|                                                          |                                                          |                                     |
|                                                          |                                                          | ``x + 0 = x = 0 + x``               |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Group                                                   | A monoid with an invertible operation                    |                                     |
| <https://en.wikipedia.org/wiki/Group_(mathematics)>`_    |                                                          |                                     |
|                                                          |                                                          | ``x * y = id = y * x``              |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+

Typeclasses
~~~~~~~~~~~

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

