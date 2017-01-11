Overview
--------

The two fundamental operations in computing are `transform` and `combine`.  All
computing tasks are performed by a combination of some form of transform and
combine primitives.  More generally, transform and combine can also be
represented by the terms `abstraction` and `composition`.

Abstraction is really a transformation technique. For example a function is an
abstraction of a concrete value. Polymorphic functions are further abstraction
of functions themselves. Composition is a combining technique, it combines
multiple objects together. A binary function combines two values together. A
monoid allows us to combine multiple objects together. For example, a repeated
addition can fold a list of integers into a sum of all its elements.
Composition can be applied to functions as well, we can combine multiple
functions together to create one combined function.

Abstraction and composition are the two fundamental and orthogonal tools of
functional computing. All functional computing is a combination of these two.
We abstract and compose at different levels to create even higher level
abstractions. It may be noteworthy that both of these are `reuse` techniques.

Abstraction
-----------

Abstraction is applied to both data and type spaces.  The most basic
abstraction is function. Functions are further abstracted using parametric and
ad-hoc polymorphism.  Typeclasses and type families provide facilities to
implement ad-hoc polymorphism in Haskell.

The fundamentals of abstraction are studied formally in a branch of mathematics
called `lambda calculus`.  Abstraction facilities in Haskell are discussed in
detail in XYZ.

Composition
-----------

We differentiate composition into two basic forms i.e. combine & fold.
Combining allows you to combine two values using a binary operation. Combining
can be used repeatedly on two or more values to arrive at a single final value.
We define folding as a more general form of combining which uses an identity
element to allow the same binary operation to combine 0 or more elements
together.

Concrete data objects are commonly composed using algebraic structures (magma
family) whereas functions are composed using categorical structures:

* Composing data - Magma (semigroup, monoid)
* Composing functions - Category (semigroupoid, category)

Composing Concrete Objects
~~~~~~~~~~~~~~~~~~~~~~~~~~

A set of data objects are composed using algebraic structures. The most important
algebraic structures for composing data are semigroup and monoid. Semigroup
allows combining two or more objects whereas a monoid allows folding zero or
more objects.

+-----------------------------------------------------------------------------+
| Algebraic structures to compose multiple objects using a binary function    |
+-------------------------------------+---------------------------------------+
| Combine                             | Fold                                  |
+=====================================+=======================================+
| Semigroup                           | Monoid                                |
+-------------------------------------+---------------------------------------+

These structures are part of `magma` family which is studied in a branch of
mathematics called `modern algebra`. For detailed description of these
composition facilities see TBD.

Composing Functions
~~~~~~~~~~~~~~~~~~~

While data objects are combined using algebraic structures, function
applications on the other hand are combined using categorical structures.  The
way a semigroup allows us to only combine two objects, similarly a semigroupoid
allows us to only combine two or more functions. A category on the other hand,
like a monoid, allows us to fold zero or more functions with the help of an
identity function.

+-----------------------------------------------------------------------------+
| Categorical structures to compose multiple functions using a binary         |
| operation called composition.                                               |
+-------------------------------------+---------------------------------------+
| Combine                             | Fold                                  |
+=====================================+=======================================+
| Semigroupoid                        | Category                              |
+-------------------------------------+---------------------------------------+

Categories are studied in a branch of mathematics called `category theory`.
For detailed description of these composition facilities see TBD.
