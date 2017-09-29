Composition, Abstraction & Combining
====================================

.. contents:: Table of Contents
   :depth: 1

+------------------------+----------------------------------------------------+
| ≡                      | Symbol for definition or equivalence               |
+------------------------+----------------------------------------------------+
| ⊕                      | Symbol representing combining two values in some   |
|                        | way (direct sum or direct product)                 |
+------------------------+----------------------------------------------------+

Overview
--------

In this chapter we do not elaborate the concepts introduced in detail. Just
treat this chapter as a roadmap to find your way and get organized. This is
more of a classifiation of concepts in a systematic manner building upon each
other. This is how the whole book has been organized.

As you go through the chapters in order, these concpets will become clearer and
then you can go through the composition chapter for a detailed summary and
illustration of all the abstract concepts and a full roadmap of all the
concepts introduced in the book.

Abstraction ~ transformation (from input domain to output domain)
Composition = Abstraction + Combining
Abstraction = Term level (functions), Type level (Polymorphism)
Combining = Product, Sum

Composition
-----------

A unary operation and a binary operation combined together are widely useful in
composing a set of values in a general manner.

transform + combine = compose

So how do we transform and combine? We use these two:
* a binary operation combines
* identity + combine = transform

Identity allows a very important special case to be handled in a generalized
manner.

A binary number system can express all numbers using two digits "0" and "1".
The same way a unary operation and binary operation together can express a
generalized n-ary operation. The unary operation, transform is our digit 0 and
the binary operation combine is the digit 1.

It is a very common pattern in Haskell.

Transformation and Combining, Distributive?
-------------------------------------------

Composition is an operation on data
Transformation is an operation on functions i.e. co-data
They get entangled with each other the same way construction and destruction
get entangled with each other.

N-ary function:
  put n things together : map the composite to another

We can first compose and then map. Is it possible to first map and then
compose?
  a x b -> c               -- just like NOT (a AND b) = (NOT a) OR (NOT b)
  (a -> c) + (b -> c)      -- that's why NOT is always needed and one of and/or
  i.e. mapping of a product is a sum of mappings
  this is just for intuition. It may not be possible for a map like a
  -> c to even exist even though (a, b) -> c exists.

  Is this possible to prove it this way?
    when we give a to a x b -> c it produces b -> c
    when we give b to a x b -> c it produces a -> c
    Now that we have given both the inputs separately and got the results we
    can combine the two complementary results to arrive at the final result.
Abstraction & Polymorphism
--------------------------

The process of supplying the values of parameters to create a concrete form is
sometimes called `instantiation` of the polymorphic form or creating an
`instance`.  Instantiation process is the opposite of abstraction.

::

  red ball, green ball, blue ball => ball + color => instances

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

------

Correspondences:

Sum and product are the most basic fundamental and dual concepts. Every type of
operation can be expressed in terms of sums or products, they help function map
from one type to another. They can also be called conjunction and disjunction
in logic programming. These concepts are fully pervasive in Haskell and can be
found in some form everywhere in every aspect, at every layer.

| Product | (a,b)      | pattern match | Function | Applicative | Monad     | Conjunction
| Sum     | Either a b | case map      | Function | Alternative | MonadPlus | Disjunction

* Note a product of types can be defined in terms of other types.
* A sum type is a sum of data constructors and not a sum of other types. A sum
  of other types would be called a coproduct.
* However the only way to define a sum type in terms of other types (i.e. a
  coproduct of types) is Either. Either is a product type that represents a sum
  of two types!
* dependent-sum package generalizes either
* dependent-map?
