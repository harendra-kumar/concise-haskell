Terminology
-----------

*  homomorphism
*  coproduct (e.g. of two monoids)

Abstract Algebra
----------------

Abstract algebra and Category theory have similar concepts but should not be
confused with each other. Category theory is more general and has generalized
concepts corresponding to algebra concepts. Most interesting structures are the
ones with associative operation.

Sets
----

+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| Structure                                                | Definition                                               | Example           |
+==========================================================+==========================================================+===================+
| `Set <https://en.wikipedia.org/wiki/Set_(mathematics)>`_ | A collection of distinct objects.                        | {1, 2, 3, 4, ...} |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+

`Functions <https://en.wikipedia.org/wiki/Function_(mathematics)>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f(X) = Y where X and Y are sets, X is domain and Y is codomain

+---------------------------+---------------------------+
| image                     | another                   |
+===========================+===========================+
| .. image:: diagr/diag.png | .. image:: diagr/diag.png |
+---------------------------+---------------------------+

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

-  **fixpoint** f(c) = c

* injective => monomorphism
* surjective => epimorphism
* bijective => bimorphism

Magma
~~~~~

+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| Structure                                                | Definition                                               | Example                             |
+==========================================================+==========================================================+=====================================+
| `Magma <https://en.wikipedia.org/wiki/Magma_(algebra)>`_ | A set with a single binary operation (closed, M × M → M) | {1, 2, 3, ...}                      |
|                                                          |                                                          |                                     |
|                                                          |                                                          | ``x1 + x2 = x3``                    |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Semigroup <https://en.wikipedia.org/wiki/Semigroup>`_   | A magma having associative binary operation              | ``(x1 + x2) + x3 = x1 + (x2 + x3)`` |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Monoid <https://en.wikipedia.org/wiki/Monoid>`_         | A semigroup with an identity element                     | {0, 1, 2, 3, ...}                   |
|                                                          |                                                          |                                     |
|                                                          |                                                          | ``x + 0 = x = 0 + x``               |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+
| `Group                                                   | A monoid with an invertible operation                    |                                     |
| <https://en.wikipedia.org/wiki/Group_(mathematics)>`_    |                                                          |                                     |
|                                                          |                                                          | ``x * y = id = y * x``              |
+----------------------------------------------------------+----------------------------------------------------------+-------------------------------------+

