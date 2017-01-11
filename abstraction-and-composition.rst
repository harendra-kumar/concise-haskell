Abstraction & Composition
=========================

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
--------------------------

The most basic composition technique is a function. A function composes its
input arguments into output. A set of data objects can be composed using
algebraic structures which utilize a function to compose. The most important
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
-------------------

The most basic composition technique for abstract objects (i.e. function
applications) is function composition. Function composition is defined by the
category, and is therefore fixed to `.` if you are working in the `Hask`
category.

While a set of data objects are combined using algebraic structures, a set of
function applications on the other hand are combined using categorical
structures.  The way a semigroup allows us to only combine two or more objects,
similarly a semigroupoid allows us to only combine two or more functions. A
category on the other hand, like a monoid, allows us to fold zero or more
functions with the help of an identity function.

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

Composing Functors
------------------

Functors are abstractions on categories like functions are abstractions on
concrete objects. They are transformations on categories. Therefore functors can
also be composed together similar to functions. The three well known techniques
to compose functors are `Applicative`, `Monad` and `Arrow` typeclasses. They are
in fact pretty similar to Monoid compositions.


Summary
-------

Every programming tool in Haskell is a combination of abstraction and
composition. The basic abstraction as well as composition technique is a
function. Every abstraction technique is some form of function which is the
basic composition tool as well. When we are composing a set of objects our basic
tool is a monoid or some form of monoid.

Parametric polymorphism and ad-hoc polymorphism are abstraction tools in the
type space. They should be thought about separately.

Basic, function like abstraction and composition tools:

+----------+----------------------------+
| Tool     | operates on                |
+==========+============================+
| function | concrete objects           |
+----------+----------------------------+
| Functor  | categories of objects      |
+----------+----------------------------+

Monoid like set composition tools:

+-------------+--------------------------------------+------------------------+
| Tool        | Operates on a set of                 | Binary composition tool|
+=============+===============+======================+========================+
| Monoid      | Haskell types | concrete objects     | functions              |
+-------------+---------------+----------------------+------------------------+
| Category    | functions     | abstract objects     | function composition   |
+-------------+---------------+----------------------+------------------------+
| Applicative | Functors      | Endofunctors         | Day convolution        |
+-------------+               +----------------------+------------------------+
| Monad       |               | Endofunctors         | function composition   |
+-------------+               +----------------------+------------------------+
| Arrow       |               | Profunctors          | function composition   |
+-------------+---------------+----------------------+------------------------+
