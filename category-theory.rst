Category Theory
===============

-  The general key point is that there is a correspondence (which
   implies difference as well) between parallel worlds. And we derive an
   operation in a world from an operation in another one by just
   defining a transformation because there is a correspondence. This
   allows reuse.

-  Categorical structures and Algebraic structures should not be mixed
   up - they can be thought of as different domains. Though there are
   parallels between the two.

Categories [Not closed]
-----------------------

In general - there are things (objects) and there are operations
(morphisms).

-  The most general structure is a semicategory or a semigroupoid. It
   has objects (things) and morphisms (operations). Morphisms (arrows)
   compose associatively. A Semigroupoid is a Category without the
   requirement of identity arrows for every object in the category.

-  A category additionally has an identity arrow (morphism) for each
   object. A Category is any Semigroupoid for which the Yoneda lemma
   holds.

-  A groupoid adds the divisibility or invertibility property as well.

--------------

-  Note that a category could be an object in a larger category
-  Within the larger category we can have morphisms from one subcategory
   to another. Such a morphism is called a Functor. For example 'List'
   or '[]' type is a morphism from Int to list of Int or Char to list of
   Char etc.
-  A Functor in Haskell maps objects and morphism (i.e. functions) in a
   subcategory of Hask to objects and morphisms of another category.

--------------

-  Functor - maps (or transforms) one category to another
-  Natural Transformation - maps one Functor to another Functor

Morphisms
~~~~~~~~~

f . g means f applied to g

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
-  the category of small categories, denoted by **Cat**, is the category
   whose objects are all small categories and whose morphisms are
   functors between categories.
-  **product category** - the product of two categories C and D, denoted
   C × D and called a product category, is a straightforward extension
   of the concept of the Cartesian product of two sets
-  A functor whose domain is a product category is known as a
   **bifunctor**.
-  a **Kleisli category** is a category naturally associated to any
   monad T. It is equivalent to the category of free T-algebras.
-  **profunctors** are a generalization of relations and also of
   bimodules. They are related to the notion of correspondences.

Algebraic Structures
--------------------

Set
~~~

Most general structure. A set is a collection of distinct objects,
considered as an object in its own right.

Magma [Closed]
~~~~~~~~~~~~~~

-  A `Magma <https://en.wikipedia.org/wiki/Magma_(algebra)>`__ consists
   of a set, M, equipped with a single binary operation, M × M → M. The
   binary operation must be closed
-  The most interesting (for haskell) specific structures derived from a
   magma are the ones with associative operations (i.e. semigroups) ->
   monoids (with identity object) -> groups (with invertibility or
   divisibility).

Parallels between categories and algebraic structures
-----------------------------------------------------

-  Associativity and Divisibility can be used exclusively or combined
   together - at least one is required in any structure.
-  Identity is additional and optional.

-  This table shows structures which **require** associativity

+-----------------+--------------------------------+---------------------------------------+
| Property        | Category Structure             | Magma (Algebraic) Structure           |
+=================+================================+=======================================+
| Closure         | Open                           | Closed                                |
+-----------------+--------------------------------+---------------------------------------+
| Associativity   | Semicategory (Semigroupoid)    | Semigroup                             |
+-----------------+--------------------------------+---------------------------------------+
| +Identity       | Category (identity morphism)   | Monoid (Identity object) (foldable)   |
+-----------------+--------------------------------+---------------------------------------+
| +Divisibility   | Groupoid                       | Group                                 |
+-----------------+--------------------------------+---------------------------------------+

Morphisms and Galois Connections
--------------------------------

Morphisms are exact one to one correspondence whereas Galois Connections
are correspondence of one object to multiple in another world. Both are
relations or correspondences only the rules of correspondence are
different. Or in other words a Galois Connections defines an object
which is an approximate or reduction of multiple objects in another
world.
