Composition, Abstraction & Polymorphism
=======================================

.. contents:: Table of Contents
   :depth: 1

+------------------------+----------------------------------------------------+
| ≡                      | Symbol for definition or equivalence               |
+------------------------+----------------------------------------------------+
| ⊕                      | Symbol representing combining two values in some   |
|                        | way (direct sum)                                   |
+------------------------+----------------------------------------------------+

Composition and Transformation
------------------------------

Every computer program is essentially a recipe to `transform` inputs to
outputs. A simple transformation of a concrete value can be achieved by
`composing` it with an abstract value that represents the transformation::

  transformation ≡ abstract ⊕ concrete

In Haskell, `composition` is a fundamental and clearly defined
building block together with `abstraction`.  We will see how each and every
step in a Haskell program is essentially a composition of values.  Thinking in
terms of composition and abstraction as fundamental building blocks helps
in understanding Haskell better.

Abstraction is really a transformation technique. For example, a function, a
transformation tool, is an abstraction of concrete values.

Composition is a combining technique, it combines multiple objects together. A
binary function, the basic composition tool, combines two values together. A
monoid allows us to combine a set of objects together. For example, a repeated
addition can fold a list of integers into a sum of all its elements.
Composition can be applied to functions as well, we can combine multiple
functions together to create one combined function.

Composition can also be viewed as a repeated transformation.

Abstraction and composition are the two fundamental and orthogonal tools of
functional computing. All functional computing is a combination of these two.
We abstract and compose at different levels to create even higher level
abstractions.

Abstraction at type level is called polymorphism. In contrast to the
abstraction at the data level, the type level abstraction is concretized at the
compile time rather than runtime.

It may be noteworthy that all the three are `reuse` techniques.

Abstraction
-----------

Abstraction is applied to both data and type spaces.  The most basic
abstraction is function. In fact, any abstraction can in general be equated
with a function or mapping.

Functions are further abstracted using parametric and ad-hoc polymorphism.
Typeclasses and type families provide facilities to implement ad-hoc
polymorphism in Haskell.

The fundamentals of abstraction are studied formally in a branch of mathematics
called `lambda calculus`.  Abstraction facilities in Haskell are discussed in
detail in XYZ.

`Abstraction vs Performance:` An abstraction is also an indirection in terms of
implementation. An indirection creates a performance overhead when the
indirection is a runtime indirection and not just compile time. For example a
function creates an indirection of a function call.  A data constructor creates
a layer of indirection. A free monad creates another layer of indirection
compared to a monad.

However, we should note that performance impacts are not always intuitive or
easily quantified. In a given scenario performance may not be important or the
impact may not be significant because of the 80/20 rule.

Composition
-----------

Composition or fold combines multiple things of the same type resulting in a
single final value. The binary operation we need to fold must have:

f :: a -> a -> a  -- dissolves a into a
f :: a -> b -> a  -- dissolves b into a
f :: a -> b -> b  -- dissolves a into b

What happens if we do not have the identity element?

5+0 = 5
5*1 = 5

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
~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~

Functors are abstractions on categories like functions are abstractions on
concrete objects. They are transformations on categories. Therefore functors can
also be composed together similar to functions. The three well known techniques
to compose functors are `Applicative`, `Monad` and `Arrow` typeclasses. They are
in fact pretty similar to Monoid compositions.


Polymorphism
------------

Polymorphism is abstraction in the type space. Function is the basic
abstraction tool even in the type space. Type functions come in different
flavors:

* polymorphic type - explicit type function
* type families - pattern matched definition of type functions - partial
* parametrically polymorphic functions - functions of types, ultimately
  instantiated to a specific type
* typeclasses - ad-hoc polymorphism - functions are decides based on types -
  partial

Summary
-------

Every programming tool in Haskell is a combination of abstraction and
composition. The basic abstraction as well as composition technique is a
function. Every abstraction technique is some form of function which is the
basic composition tool as well. When we are composing a set of objects our basic
tool is a monoid or some form of monoid.

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

Semigroup Monoid   Accumulate  Recursion    List/Tree finite    open   fold  Sequence/Applicative     Tree/Monad     List
         CoMonoid  Copy/Split Co-recursion  Stream    infinite  closed fold  Sequence/Co-applicative  Tree/Co-monad  StateMachine

Applicative is a fold of sequences.
Monad/Comonad are folds of trees.

