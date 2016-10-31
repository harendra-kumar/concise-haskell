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

Algebraic Structures
~~~~~~~~~~~~~~~~~~~~

+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| Structure                                                | Definition                                               | Example           |
+==========================================================+==========================================================+===================+
| `Set <https://en.wikipedia.org/wiki/Set_(mathematics)>`_ | A collection of distinct objects.                        | {1, 2, 3, 4, ...} |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Magma <https://en.wikipedia.org/wiki/Magma_(algebra)>`_ | A set with a single binary operation (closed, M × M → M) | Addition          |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Semigroup <https://en.wikipedia.org/wiki/Semigroup>`_   | A magma where the binary operation is associative        | Addition          |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Monoid <https://en.wikipedia.org/wiki/Monoid>`_         | A semigroup with an identity element.                    | {0, 1, 2, 3, ...} |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+
| `Group                                                   |                                                          |                   |
| <https://en.wikipedia.org/wiki/Group_(mathematics)>`_    | A monoid with inverse elements.                          |                   |
+----------------------------------------------------------+----------------------------------------------------------+-------------------+

Category Theory
---------------

A category C consists of objects (denoted as a, b etc.) and morphisms (aka
arrows, denoted as f, g etc.) and a binary operation called composition of
morphisms (denoted as f . g and read as f after g).

* small category: objects and morhphims are sets and not proper classes
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

Some intereseting categories:

+---------+--------------------------------+---------------+
| Category| Objects                        | Morphisms     |
+=========+================================+===============+
| Set     | sets                           | functions     |
+---------+--------------------------------+---------------+
| Vect(K) | vector spaces over the field K | K-linear maps |
+---------+--------------------------------+---------------+
| Cat     | small categories               | functors      |
+---------+--------------------------------+---------------+

* Functor: A morphism from one (small) category to another
* Natural Transformation: A morphism from a functor to another

Morphisms
~~~~~~~~~

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

-  **fixpoint** f(c) = c
-  an **injective** function or injection or one-to-one function is a
   function that preserves distinctness: *it never maps distinct
   elements of its domain to the same element of its codomain*. A
   monomorphism is a generalization of an injective function in category
   theory.
-  a function f from a set X to a set Y is **surjective** (or onto), or
   a surjection, if every element y in Y has a corresponding element x
   in X such that f(x) = y. The function f may map more than one element
   of X to the same element of Y. *Surjective - every element is
   mapped-to*. An epimorphism is a generalization of a surjective
   function in category theory.
-  a **bijective** function f: X → Y is a one-to-one (injective) and
   onto (surjective) mapping of a set X to a set Y. A bimorphism (and
   sometimes isomorphism?) is a generalization of a bijective function
   in category theory.
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
