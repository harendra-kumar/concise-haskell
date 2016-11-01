Category Theory
===============

The key point is that there is a correspondence (which
implies difference as well) between parallel worlds. And we derive an
operation in a world from an operation in another one by just
defining a transformation because there is a correspondence. This
allows reuse.

Branches of mathematics
-----------------------

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

+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| Structure                                                | Definition                                               | Example           |
+==========================================================+==========================================================+===================+
| `Magma <https://en.wikipedia.org/wiki/Magma_(algebra)>`_ | A set with a single binary operation (closed, M × M → M) | Addition          |
|                                                          |                                                          |                   |
|                                                          | x1 + x2 = x3                                             |                   |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Semigroup <https://en.wikipedia.org/wiki/Semigroup>`_   | A magma where the binary operation is associative        | Addition          |
|                                                          |                                                          |                   |
|                                                          | (x1 + x2) + x3 = x1 + (x2 + x3)                          |                   |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Monoid <https://en.wikipedia.org/wiki/Monoid>`_         | A semigroup with an identity element.                    | {0, 1, 2, 3, ...} |
|                                                          |                                                          |                   |
|                                                          | x + 0 = x                                                |                   |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Group                                                   | A monoid with inverse elements.                          |                   |
| <https://en.wikipedia.org/wiki/Group_(mathematics)>`_    |                                                          |                   |
|                                                          | x * y = id                                               |                   |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+

Category Theory
---------------

A category C consists of objects (denoted as ``a``, ``b`` etc.) and morphisms
(aka arrows, denoted as ``f``, ``g`` etc.) and a binary operation called
``composition`` of morphisms (denoted as ``f . g`` and read as f after g).

* small category: objects and morphisms are sets and not proper classes
* large category: objects and morphisms are proper classes

A category is not closed under the binary operation unlike algebraic
structures.

+-----------------------------+----------------------------+---------------------+
| Category Structure          | Definition                 | Corresponding       |
|                             |                            | algebraic structure |
+=============================+============================+=====================+
| Semicategory (Semigroupoid) | Composition is associative | Semigroup           |
+-----------------------------+----------------------------+---------------------+
| (Small) category            | Composition has identity   | Monoid              |
+-----------------------------+----------------------------+---------------------+
| Groupoid                    | Composition has inverses   | Group               |
+-----------------------------+----------------------------+---------------------+

Some interesting categories:

+---------+--------------------------------+---------------+
| Category| Objects                        | Morphisms     |
+=========+================================+===============+
| Set     | sets                           | functions     |
+---------+--------------------------------+---------------+
| Vect(K) | vector spaces over the field K | K-linear maps |
+---------+--------------------------------+---------------+
| Cat     | small categories               | functors      |
+---------+--------------------------------+---------------+

Special morphisms:

* Functor: A morphism from one (small) category to another
* Natural Transformation: A morphism from a functor to another

Functions & Morphisms
~~~~~~~~~~~~~~~~~~~~~

-  Monomorphism - Never combines with two different operations to
   produce the same result (f where f . g1 /= f . g2). generalization of
   injective function.
-  Epimorphism - Two different morphisms combined with this morphism can
   never produce the same result (g where f1 . g /= f2 . g).
   generalization of surjective function.
-  Bimorphism = Epi + Mono . generalization of bijective.
-  isomorphism - f: X → Y, g: Y → X such that f ∘ g = idY and g ∘ f =
   idX. f and g are isomorphisms and inverses.
-  endomprhism - f: X → X is an endomorphism of X
-  Automorphism = iso + endo

Inverses
~~~~~~~~

f . g = id

-  f = retraction = left inverse => Epimorphism
-  g = section = right inverse => Monomorphism
-  left = right => Bimorphism

Terminology
-----------

-  a **2-category** is a category with "morphisms between morphisms";
   that is, where each hom-set itself carries the structure of a
   category.
-  **product category** - the product of two categories C and D, denoted
   C × D and called a product category, is a straightforward extension
   of the concept of the Cartesian product of two sets
-  A functor whose domain is a product category is known as a
   **bifunctor**.
-  a **Kleisli category** is a category naturally associated to any
   monad T. It is equivalent to the category of free T-algebras.
-  **profunctors** are a generalization of relations and also of
   bimodules. They are related to the notion of correspondences.

Haskell
~~~~~~~

- Hask is a category with Haskell types as objects and functions as morphisms.
- For example, 'List' or '[a]' is a functor type which maps morphisms on type a to
  morphisms on type [a] via fmap.

Morphisms and Galois Connections
--------------------------------

Morphisms are exact one to one correspondence whereas Galois Connections
are correspondence of one object to multiple in another world. Both are
relations or correspondences only the rules of correspondence are
different. Or in other words a Galois Connections defines an object
which is an approximate or reduction of multiple objects in another
world.
