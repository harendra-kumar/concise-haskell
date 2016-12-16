Category Theory
===============

Terminology
-----------

+------------------+----------------------------------------------------------+
| hom-set          |                                                          |
+------------------+----------------------------------------------------------+
| 2-category       | A category with "morphisms between morphisms"; that      |
|                  | is, where each hom-set itself carries the structure of a |
|                  | category.                                                |
+------------------+----------------------------------------------------------+
| Product category | The product of two categories C and D, denoted C × D and |
|                  | called a product category, is a straightforward extension|
|                  | of the concept of the Cartesian product of two sets      |
+------------------+----------------------------------------------------------+
| Bi-functor       | A functor whose domain is a product category             |
+------------------+----------------------------------------------------------+
| Kleisli category | A category naturally associated to any monad T.          |
+------------------+----------------------------------------------------------+
| Profunctor       | A generalization of relations and also of bimodules.     |
|                  | They are related to the notion of correspondences.       |
+------------------+----------------------------------------------------------+

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
| Hask    | types                          | functions     |
+---------+--------------------------------+---------------+

Morphisms
~~~~~~~~~

Morphisms are a more general concept than functions. For example a morphism
could be a relation (e.g. greater than) rather than a function.

+--------------+--------------+------------------+----------------------------+
| Morphism     | Function Eq. | Rule             | Description                |
+==============+==============+==================+============================+
| Monomorphism | Injective    | f . g1 /= f . g2 | Maps to unique results     |
|              |              |                  +----------------------------+
|              |              |                  | Unique right inverse exists|
+--------------+--------------+------------------+----------------------------+
| Epimorphism  | Surjective   | g1 . f /= g2 . f | Produces unique results    |
|              |              |                  | when mapped                |
|              |              |                  +----------------------------+
|              |              |                  | Unique left inverse exists |
+--------------+--------------+------------------+----------------------------+
| Bimorphism   | Bijective    | Epi + Mono       | Both left and right        |
|              |              |                  | inverse are unique         |
+--------------+--------------+------------------+----------------------------+
| Isomorphism  |              | f . g = idY      | f: X -> Y, g: Y -> X       |
|              |              |                  |                            |
|              |              | g . f = idX      | f and g are isomorphisms   |
|              |              |                  | and inverses               |
+--------------+--------------+------------------+----------------------------+
| Endomprhism  |              | f: X → X         |                            |
+--------------+--------------+------------------+----------------------------+
| Automorphism |              | Endo + Iso       |                            |
+--------------+--------------+------------------+----------------------------+

Inverses
~~~~~~~~

* retraction: left inverse
* section: right inverse

Special morphisms
-----------------

+------------------------+----------------------------------------------------+
| Functor                | A morphism from one (small) category to another    |
+------------------------+----------------------------------------------------+
| Natural Transformation | A morphism from a functor to another               |
+------------------------+----------------------------------------------------+

Haskell
~~~~~~~

- Hask is a category with Haskell types as objects and functions as morphisms.
- For example, 'List' or '[a]' is a functor type which maps morphisms on type a to
  morphisms on type [a] via fmap. Type a denotes one category and type [a]
  denotes another category.
