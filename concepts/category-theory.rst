Categorical Composition
=======================

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
| Coproduct        |                                                          |
+------------------+----------------------------------------------------------+
| Bi-functor       | A functor whose domain is a product category             |
+------------------+----------------------------------------------------------+
| Profunctor       | Generalizes functors the way relations generalize        |
|                  | functions.                                               |
+------------------+----------------------------------------------------------+
| Kleisli category | A category naturally associated to any monad T.          |
+------------------+----------------------------------------------------------+
| Monoidal Category| A monoidal category is a category equipped with a        |
|                  | tensor product and a unit object.                        |
|                  | Monoidal categories can be used to define the concept    |
|                  | of a monoid object and an associated action on the       |
|                  | objects of the category                                  |
+------------------+----------------------------------------------------------+
| Catamorphism     | the unique homomorphism from an initial algebra into     |
|                  | some other algebra.                                      |
|                  | catamorphisms provide generalizations of folds of lists  |
|                  | to arbitrary algebraic data types                        |
+------------------+----------------------------------------------------------+
| Anamorphism      | The categorical dual of catamorphism. Generalizes unfolds|
+------------------+----------------------------------------------------------+
| F-Algebra        |                                                          |
+------------------+----------------------------------------------------------+

* comma category
* limits and colimits
* adjunctions

* tensorial strength - natural transformation preserving certain structure of
  the functor is the strength of the functor.

Layman description
------------------

The key point is that there is a correspondence (which
implies difference as well) between parallel worlds. And we derive an
operation in a world from an operation in another one by just
defining a transformation because there is a correspondence. This
allows reuse.

Illustrate category theory with an example of the number system. How we can
apply a solution from one field to another by abstracting it. How relationships
among different objects/concepts are important and the specific details of the
objects are just noise.

Morphisms and Galois Connections
--------------------------------

Morphisms are exact one to one correspondence whereas Galois Connections
are correspondence of one object to multiple in another world. Both are
relations or correspondences only the rules of correspondence are
different. Or in other words a Galois Connections defines an object
which is an approximate or reduction of multiple objects in another
world.

Definition by relations
-----------------------

In category theory an object is uniquely defined by its relations with other
objects. There is nothing absolute, everything is relative. So you do not need
a reference frame. Things are always in relation to other things and those
relations define what they are. This true about everything and can be applied
to things in everyday life and anything in the universe.

That is one of the reasons why we unnderstand concepts better when we think
about their duals. We think of a concept in relation to its dual. For example
we understand 0 and infinity better when we think about them both as duals of
each other rather than when we only think about one and not the other.

Objects are immaterial
----------------------

In categories we have nothing to do with the objects per se they are opaque to
us. Objects are only the endpoints of functions. We are only operating on the
functions more generally known as morphisms in category theory.

Universal Construction
~~~~~~~~~~~~~~~~~~~~~~

There is a common construction in category theory called the universal
construction for defining objects in terms of their relationships. It is
universal because the objects do not matter. The construction holds for any
object. As long as the relationships are the same we have the same
construction. Its a generalized construction irrespective of what our objects
are. It is an abstraction, meaning the objects do not matter we have an
abstract concept defined just by the relationships among objects.

For example a product type for sets is defined as:

data (a, b) = (a, b)

The relationships or morphisms or functions between the constituent sets a and
b and the product (a,b) are:

fst (a, b) = a
snd (a, b) = b

Now, if any opaque object satisfies the same relationships then we have a
generalized product construction. Does not matter what the objects are if they
satisfy fst and snd type of relationships we have a product of the two objects.

A product of two objects a and b is the object c equipped with two projections
such that for any other object c’ equipped with two projections there is a
unique morphism m from c’ to c that factorizes those projections.

coproduct is the dual of product.

A coproduct of two objects a and b is the object c equipped with two injections
such that for any other object c’ equipped with two injections there is a
unique morphism m from c to c’ that factorizes those injections.

A product behaves like multiplication, with the terminal object playing the
role of one; whereas coproduct behaves more like the sum, with the initial
object playing the role of zero. In particular, for finite sets, the size of
the product is the product of the sizes of individual sets, and the size of the
coproduct is the sum of the sizes.

------

left :: a -> Either a b is the dual of the projection fst :: (a,b) -> a

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

A Category is any Semigroupoid for which the Yoneda lemma holds.

Examples
--------

+---------+--------------------------------+---------------+
| Category| Objects                        | Morphisms     |
+=========+================================+===============+
| Set     | sets                           | functions     |
+---------+--------------------------------+---------------+
| Cat     | small categories               | functors      |
+---------+--------------------------------+---------------+
| Hask    | types                          | functions     |
+---------+--------------------------------+---------------+

Morphisms
~~~~~~~~~

Morphisms are more general than functions. For example a morphism could be a
relation (e.g. a "greater than" relationship) rather than a function.

+--------------+--------------+------------------+----------------------------+
| Morphism     | Function Eq. | Rule             | Description                |
+==============+==============+==================+============================+
| Monomorphism | Injective    | f . g1 /= f . g2 | Maps one to one            |
| (f)          |              |                  +----------------------------+
|              |              |                  | Unique right inverse exists|
+--------------+--------------+------------------+----------------------------+
| Epimorphism  | Surjective   | g1 . f /= g2 . f | Produces unique results    |
| (f)          |              |                  | when mapped                |
|              |              |                  +----------------------------+
|              |              |                  | Unique left inverse exists |
+--------------+--------------+------------------+----------------------------+
| Bimorphism   | Bijective    | Epi + Mono       | Both left and right        |
|              |              |                  | inverse are unique         |
+--------------+--------------+------------------+----------------------------+
| Isomorphism  |              | f . g = idY      | f: X -> Y, g: Y -> X       |
| (f & g)      |              |                  |                            |
|              |              | g . f = idX      | f and g are isomorphisms   |
|              |              |                  | and inverses               |
+--------------+--------------+------------------+----------------------------+
| Endomprhism  |              | f: X → X         |                            |
| (f)          |              |                  |                            |
+--------------+--------------+------------------+----------------------------+
| Automorphism |              | Endo + Iso       |                            |
+--------------+--------------+------------------+----------------------------+

Inverses
~~~~~~~~

When an operation maps more than one values to a single value an inverse will
not be unique. In case of functions this means inverse will not be a valid
function.

* retraction: left inverse
* section: right inverse

Special morphisms
-----------------

+------------------------+----------------------------------------------------+
| Functor                | A morphism from one (small) category to another    |
+------------------------+----------------------------------------------------+
| Natural Transformation | A morphism from a functor to another functor       |
+------------------------+----------------------------------------------------+

Functors
--------

* Endofunctor - a functor from a category to itself

Products and Coproducts
-----------------------

A product behaves like multiplication, with the terminal object playing the
role of one; whereas coproduct behaves more like the sum, with the initial
object playing the role of zero. In particular, for finite sets, the size of
the product is the product of the sizes of individual sets, and the size of the
coproduct is the sum of the sizes.

Product
~~~~~~~

In Haskell a product of two types can be represented as::

  data (a, b) = (a, b)

Product is "AND" or conjunction - it has two elements - say, `x` from `a` AND
`y` from `b`.

Coproduct or Sum
~~~~~~~~~~~~~~~~

The dual of product is coproduct. A coproduct represents a choice. It can be
represented by the `Either` data type in Haskell::

  data Either a b = Left a | Right b

Coproduct is "OR" or disjunction - it has one element which is - either `x`
from `a` OR `y` from `b`.

Hakell Typeclasses
------------------

+-----------------------------------------------------------------------------+
| Categorical structures to compose multiple functions using a binary         |
| operation called composition.                                               |
+-------------------------------------+---------------------------------------+
| Combine                             | Fold                                  |
+=====================================+=======================================+
| Semigroupoid                        | Category                              |
+-------------------------------------+---------------------------------------+

Hask - Category of Haskell Types
--------------------------------

Haskell types and (monomorphic) functions form a category which is called `Hask`.

+-----------------------------------------------------------------------------+
| Category Hask                                                               |
+=====================+=======================================================+
| Objects             | types                                                 |
+---------------------+-------------------------------------------------------+
| Morphisms           | functions                                             |
+---------------------+-------------------------------------------------------+
| combining operation | function composition                                  |
+---------------------+-------------------------------------------------------+

Functors in Hask
~~~~~~~~~~~~~~~~

In Haskell a functor maps functions from category Hask to Hask. Therefore every
instance of a functor in Hask is an endofunctor.

For example, list type `[]` is a `Functor` type because it provides `map` which
is a function to map any function from type `a` to type `[a]`.  For example,
`Int` and `[Int]` are two types in Hask, `map succ [1,2,3]` maps the function
`succ` which works on `Int` to work on `[Int]`.

Natural Transformations
~~~~~~~~~~~~~~~~~~~~~~~

An `Applicative` functor in Haskell provides two natural transformations i.e.
`pure` and `<*>` to transform the functor.

Monoidal Category of Endofunctors
---------------------------------

* objects - functors
* morphism - natural transformation
* combine and fold - natural transformations
* combining operation - tensor

In category theory, preservation of monoidal structure is related to tensorial
strength, so an applicative functor is also known as a strong lax monoidal
functor. However, in Hask, every functor has canonical strength with respect to
the product, so this property doesn't add anything to the definition.


For example an applicative is a functor with two natural transformations to
preserve the monoidal structure.

Notions of Computation
----------------------

Computation involves combining or folding functions or computations together.
The most convenient and powerful way to fold is using a monoid.

+-----------------------------------------------------------------------------+
| Each one of the three well-known notions of computations in Haskell are a   |
| monoid in a monoidal category.                                              |
+-------------+--------------+------------------------------------------------+
| Structure   | Monoidal     | Description                                    |
|             | category     |                                                |
+=============+==============+================================================+
| Applicative | Endofunctors | lax monoidal functors with a compatible        |
|             |              | strength.                                      |
|             |              | Monoids in a monoidal category of endofunctors |
|             |              | using Day convolution as a tensor.             |
+-------------+--------------+------------------------------------------------+
| Arrows      | Profunctors  | Strong monoids in a monoidal category of       |
|             |              | profunctors.                                   |
+-------------+--------------+------------------------------------------------+
| Monad       | Endofunctors | Monoids in a monoidal category of endofunctors |
|             |              | using composition as a tensor.                 |
+-------------+--------------+------------------------------------------------+

The main difference between monads and applicative functors is that the latter
does not allow effects to depend on previous values, i.e. they are fixed
beforehand.

* A normal function could be wrapped in a (applicative) type and applied
  to values wrapped in the same type. This is another way of composing
  which is a transformation on normal function application to the peculiar
  world of the type.
* An applicative allows you to compose functor applications, monoidal functor
* A monad allows you to compose by chaining or collecting and using the results
  in a certain way. A do block in Monad allows you to chain conveniently. In
  monad you first compose the actions and then run them using an input.

Applicative, Monad, Arrows all provide facilities to compose computations and
that is why the corresponding modules are in `Control.*` namespace. On the
other hand a Functor allows transformation of a type and its operations to
another and is therefore under `Data.*` namespace.

References
----------

* https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/
* https://stackoverflow.com/questions/43572970/is-coproduct-the-same-as-sum-types
* https://stackoverflow.com/questions/14249955/why-isnt-there-a-simple-syntax-for-coproduct-types-in-haskell
