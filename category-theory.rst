Category Theory
===============

Terminology
-----------

* classes

Category
--------

A category C consists of objects (denoted as ``a``, ``b`` etc.) and morphisms
(aka arrows, denoted as ``f``, ``g`` etc.) and a binary operation called
``composition`` of morphisms (denoted as ``f . g`` and read as `f after g`).

* small category: objects and morphisms are sets and not proper classes
* large category: objects and morphisms are proper classes

A category is not closed under the binary operation unlike algebraic
structures.

+-----------------------------+-------------------------------------------+---------------------+
| Category Structure          | Definition                                | Corresponding       |
|                             |                                           | algebraic structure |
+=============================+===========================================+=====================+
| Semicategory (Semigroupoid) | Composition is associative                | Semigroup           |
|                             |                                           |                     |
|                             | ``f . (g . h) = (f . g) . h``             |                     |
+-----------------------------+-------------------------------------------+---------------------+
| (Small) category            | Composition has identity                  | Monoid              |
|                             |                                           |                     |
|                             | ``f . id = f = id . f``                   |                     |
+-----------------------------+-------------------------------------------+---------------------+
| Groupoid                    | Every morphism is invertible or the       |                     |
|                             | category can be viewed as having an       | Group               |
|                             | associated unary operation called         |                     |
|                             | ``inverse``                               |                     |
+-----------------------------+-------------------------------------------+---------------------+

Categories
----------

+---------+--------------------------------+---------------+
| Category| Objects                        | Morphisms     |
+=========+================================+===============+
| Set     | sets                           | functions     |
+---------+--------------------------------+---------------+
| Vect(K) | vector spaces over the field K | K-linear maps |
+---------+--------------------------------+---------------+
| Cat     | small categories               | functors      |
+---------+--------------------------------+---------------+

Special morphisms
-----------------

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
-  isomorphism - f: X -> Y, g: Y -> X such that f . g = idY and g . f =
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
