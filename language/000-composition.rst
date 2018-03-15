Denotational Composition
========================

.. Title "Beautiful Composition" will sound less erudite and more poulist

.. “The best programs are written so that computing machines can perform them
  quickly and so that human beings can understand them clearly.

"A programmer is ideally an essayist who works with traditional aesthetic and
literary forms as well as mathematical concepts, to communicate the way that an
algorithm works and to convince a reader that the results will be correct.” ―
Donald Ervin Knuth

.. contents:: Table of Contents
   :depth: 1

.. sectnum::

Terminology
-----------

+------------------------+----------------------------------------------------+
| ≡                      | Symbol for definition or equivalence               |
+------------------------+----------------------------------------------------+
| ⊕                      | Symbol representing combining two values in some   |
|                        | way (direct sum or direct product)                 |
+------------------------+----------------------------------------------------+
| Logic                  | Computing logic, processing logic                  |
+------------------------+----------------------------------------------------+
| Data                   | Contents of memory used for computing              |
+------------------------+----------------------------------------------------+
| Transform              | Transform a data entity to another using some      |
|                        | transform specific computing logic.                |
+------------------------+----------------------------------------------------+
| Combine                | Combine data entities together to create a single  |
|                        | combined entity.                                   |
+------------------------+----------------------------------------------------+
| Product                | A way of combining, AND                            |
+------------------------+----------------------------------------------------+
| Sum                    | A way of combining, OR                             |
+------------------------+----------------------------------------------------+
| Conjunction            | A way of combining, AND, Product                   |
+------------------------+----------------------------------------------------+
| Disjunction            | A way of combining, OR, Sum, union                 |
+------------------------+----------------------------------------------------+
| Term Level             |                                                    |
+------------------------+----------------------------------------------------+
| Type Level             |                                                    |
+------------------------+----------------------------------------------------+
| Denotation             | Meaning of a term or expression                    |
+------------------------+----------------------------------------------------+
| Denotational semantics | Each term or expression has a well defined         |
|                        | denotation or meaning, in some universe of         |
|                        | mathematical meanings.                             |
+------------------------+----------------------------------------------------+
| Equational reasoning   | Reasoning about a program just like mathematical   |
|                        | equations by substituting terms with their         |
|                        | definitions.                                       |
+------------------------+----------------------------------------------------+

Overview
--------

In this chapter we do not elaborate the introduced concepts in detail.  Treat
this chapter as a roadmap to find your way and get organized. This is more of a
classification of concepts in a systematic manner showing how they build upon
each other. This also gives an idea how the whole book is organized.

As you go through the chapters in order, these concepts will become clearer and
then you can go through this chapter once again for a detailed summary and
illustration of all the abstract concepts and a full roadmap of all the
concepts introduced in the book.

This is a big picture chapter that one may not be able to fully understand and
appreciate without learning the basics of the language. But you can keep coming
back to this to reinforce the big picture as you learn more and more of the
details.

Denotational vs Operational Semantics
-------------------------------------

Denotational semantics involves constructing mathematical objects (called
denotations) that describe the meanings of expressions from the programming
language. An important tenet of denotational semantics is that semantics should
be compositional: the denotation of a program phrase should be built out of the
denotations of its subphrases. In other words denotational semantics mean full
modularity and composability.

+------------------------------------+----------------------------------------+
| Denotational                       | Operational                            |
+====================================+========================================+
| What a program does                | How it does it                         |
+------------------------------------+----------------------------------------+
| Specification                      | Implementation & Execution             |
+------------------------------------+----------------------------------------+
| Reason about the meaning and       | Reason about performance               |
| correctness                        |                                        |
+------------------------------------+----------------------------------------+
| Equational reasoning for           | Space leaks, Optimal evaluation,       |
| correctness                        | Garbage collection                     |
+------------------------------------+----------------------------------------+
| Architect                          | Engineer                               |
+------------------------------------+----------------------------------------+

Pure Functional vs Imperative Paradigm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pure functional and imperative language designs are duals to each other.
Therefore, where they excel and where they lack is opposite of each other.
Functional paradigm is strong in denotational aspects whereas imperative
paradigm is strong in operational aspects:

* Haskell enforces referential transparency but adds manual (by discipline)
  control over some operational semantics, by using strictness annotations for
  example.
* Imperative languages have better control over operational semantics by
  default but allow referential transparency only by discipline.

+------------------------------+----------------------------+
| Haskell (or pure functional) | Imperative languages       |
+==============================+============================+
| Denotational first           | Operational first          |
+------------------------------+----------------------------+

Haskell Semantic Features
~~~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+--------------------------+---------------------------+
| User level feature   | Language feature         | Underlying enabler        |
+======================+==========================+===========================+
| Equational Reasoning | Denotational Semantics   | Referential Transparency  |
+----------------------+--------------------------+---------------------------+
| Expressive Power     | Infinite data structures | Non-strict semantics      |
+----------------------+                          |                           |
| Modularity           |                          |                           |
+----------------------+--------------------------+---------------------------+

Terms and Types
---------------

.. Refer to the, expressions and equations, transform and combine chapters for
   more details.

A Haskell program constitutes of `expressions` which in turn are composed of
`terms`. The part of the program that deals with terms and expressions is
called the `term level program`. There is also a part of the Haskell program that
deals with `types` of terms rather than terms themselves, we call it the `type
level program`.

Composing Terms
---------------

.. Abstraction = Term level (functions), Type level (Polymorphism)
.. Composition = Containers -| Transformations

A Haskell program is a composition of terms.  In this section we summarize the
composition facilities available for term level programming.  Haskell provides
principled composition facilities built from just two fundamental conceptual
primitives,  we will call them a `container` and a `transformation`. A
container combines multiple elements of data together and a transformation
transforms one container to another. A container is a `functor` and
`transformation` is a `function`. We will later see that even
transformations can be containers or functors. In that case, we can treat all
composition simply as composition of functors. We will see a beautiful pattern
of composition primitives across different layers of abstraction involving a
combination of just these two fundamental primitives, and other higher level
primitives built on top of these.

We divide the composition facilities in three different layers in increasing
order of abstraction. At each layer we divide them in two dimensions viz.
`product` and `sum` style composition which are two fundamental ways to combine
objects.

..
  TBD: Pictures. Values round or triangles.
    - functions >
                >=>
                >
    - Use colors to distinguish types
    - functors will have double edges
    - the color of the functor edge different than the value
    - same functor means same color edge
    - join the functor edges to show functor composition (second track)

Algebraic Data: Pure Containers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first and the simplest layer of composition is the pure data composition
layer which provides ability to define pure data containers in their simplest
form and combining them.  It is the purest way of combining without any
transformation or abstraction involved.  Pure data containers provide us
facilities to store or retrieve data, it is like the memory of a computer.

There are two elementary ways to combine data, `sum` and `product`, algebraic
data types also provide these two ways of combining. The sum type composition
either creates a new data type from scratch as a collection of choices, or
unions the choices of existing data types under a new data type. Notice that
all the choices in a sum denote the same type.  On the other hand, the product
type is used to multiply the choices (i.e.  enumerate all combinations) of two
or more existing data types. Sum is the most fundamental way to combine and
product can be thought of as a convenient tool to perform repeated sum.  Notice
that any sum style collection contains all elements of the same type whereas a
product style collection can be heterogeneous, and may have elements of the same
or different types.

.. More details in chapter "transform and combine", ADTs.

+-----------------------------------------------------------------------------+
| Composition by Algebraic Data Types (pure combining)                        |
+============================+================================================+
| Sum                        | data Bool = True | False                       |
+----------------------------+------------------------------------------------+
| Product                    | data Pair = Pair Bool Bool                     |
+----------------------------+------------------------------------------------+

Functions: Pure Transformations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. Abstraction ~ transformation (from input domain to output domain)

.. A computer program is simply a recipe to transform a number of inputs to some
  output.  Any Haskell composition transforms its inputs into a single output
  using precisely two conceptual primitives viz. `combine` and `transform`.

A pure transform is a mapping from one data type to another, a unary function
in mathematical terms. N-ary functions compose `n` inputs into one output type
that we call a product of the inputs. However, at the primitive level we can
express an `n-ary product` in terms of a pure combine followed by a
series of unary transforms.  An `adjunction` represents a transform as any
other data and allows us to combine each element from an input data container
with the transform, one at a time, the result of this combining is an output
data item that is a transform of reduced arity, thus providing the `curry`
operation and its higher order equivalents.  This fine granular joining of
combine and transform operations allows full modularity of composition.

This is the essence of what we call a `product` style composition where we
compose two or more possibly different types into a single output type.  An
n-ary function being the prototypical example of such a composition.  This same
concept of product style composition can be extended to the functor level
abstraction as well.

.. We will see later that the nested loop pattern from imperative style
  programming is also a form of product style composition.

.. Note that we use the term composition here in a general sense and not in the
  specific sense of function composition or like. It denotes any operation that
  takes one or more objects as inputs and generates one output object.

.. The opposite of transformation is `asbtraction`. Abstraction is what the
  programmer does when writing a program. For example, creating a function is
  creating an abstraction, when we reverse the process and apply the arguments
  to a function we call it transformation.  At the type level creating a
  type function (polymorphics type) or a type class is an abstraction that is
  known as polymorphism in the type context.

.. combining or product style composition creates a nesting. A function of
  multiple arguments is an implicit product of its arguments. Arity is the
  level of nesting. When we apply an argument it destroys one nest level. So
  combining instructs the transform to act in a nested manner like nested "for"
  loops. We apply multiple transforms in a nested fashion. So we have nested
  case expressions for each level as well. A pure transform or a single
  argument function has no nesting.

.. In boolean logic, NAND and NOR logical connectives are functionally
   complete. A transform is the logical equivalent of an implication ("->"),
   the product style combine operation is equivalent to AND, and the sum
   combine operation is equivalent to OR. When we include bottom ("_|_") as
   well we have a "functionally complete" set of connectives as NOT is
   equivalent to "A -> _|_". See the type-theory chapter. It should be noted
   that any one of the two combine operation is sufficient for functional
   completeness.

.. TODO picture
  1) input type -> transform -> output type : pure transform
  2) typeA + typeB -> combine -> output typeC : pure combine
  2) typeA + typeB -> combine-and-transform -> output typeC

The product style composition combines finite number of objects of possibly
different types in a custom manner i.e. the mapping can be freely defined by
the programmer. Further using this primitive style,
we can combine arbitrary number of objects occurring in a
pattern. For example a `semigroup` style composition uses a binary operation
(i.e. a product style composition) to combine two objects of the same type into
another object of the same type. A semigroup composition can be used repeatedly
to combine the resulting object with the next object in the input structure,
and so on thus folding the whole structure into a single object. A `monoid`
style composition is a further specialization of semigroup where we always
require a representation for an empty or initial target container and therefore
we can fold `zero or more` elements instead of `one or more` in case of a
semigroup.  We call such recursive compositions `folds` or sometimes `sum` as
the types of objects being combined are all the same.

.. These structures are part of `magma` family which is studied in a branch of
  mathematics called `modern algebra`. Refer to the Algebra chapter.

.. Sum: when the objects being folded are of the same type we also call it a
   sum. There is a addition symbol indicating this in many cases e.g. "++",
   "mplus", "Plus", and functions like "asum" or "msum".

+------------------------------------------------------------+
| Composition                                                |
+====================+=======================================+
| Product            | Adjunction (Combine, Transform)       |
+--------------------+------------------+--------------------+
| Fold or sum        | Semigroup        | Monoid             |
+--------------------+------------------+--------------------+

All composition can be divided into these two fundamental styles (product and
sum) occurring in different forms at different levels of abstractions.  In the
following table we summarize various constructs that are available at the
`pure function level abstraction` (also called pure or effect-free) in both
these styles. In the forthcoming chapters we will develop these constructs in
more details.

`Lift` is an operation that `lifts` a type into the current type. In case of
pure containers a `Lift` is just a constructor. In case of functions `const`
lifts any value to a function.

+------------------------------------------------------+
| Unary Transforms                                     |
+=================================+====================+
| Lift                            | const              |
+----------------+----------------+--------------------+
| Product        | Input          | Extend             |
|                +----------------+--------------------+
|                | Output         | Bind               |
+----------------+----------------+--------------------+
| Fold           | Semigroupoid   | .                  |
|                +----------------+--------------------+
|                | Category       | id                 |
+----------------+----------------+--------------------+

+-----------------------------------------------------+
| Composing Values                                    |
+=================================+===================+
| Lift (Free product)             | (,)               |
+---------------------------------+-------------------+
| Product                         | N-ary             |
| (Adjunction)                    | Application       |
+----------------+----------------+-------------------+
| Sum (Fold)     | Semigroup      | <>                |
|                +----------------+-------------------+
|                | Monoid         | mempty            |
+----------------+----------------+-------------------+

Functors: Generalizing Containers and Transformations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
..
  Applicative creates product of values in the same functor.
  Arrows is function composition in the same functor.
  Profunctor is general function composition with independent compositions at both ends.
  Monad creates products of functions in the same functor.

  Applications (Apply, Bind) are effectful.
  Include co structures?

  * Arrow requires Category constraint
  * Functors lift the earlier abstractions to one level up and allow effectful composition
  * Only a multi-functor can have a choice, does not make sense for a single parameter functor because no choice exists.
  * In a functor a regular function has to be lifted for application to the functor value
  * In an applicative values also have to be lifted for application

In general, we can express all composition in terms of containers or `functors`
and unary transform operations (natural transformations) on containers (later
we will see that even a transform can be expressed as a data container).
Functorial composition is a more general form of pure composition. The pure
composition that we discussed in the previous section can be considered as a
special case of functorial composition where the functor is an `Identity`
functor.

.. details - show that by replacing the functor by an identity functor we
  arrive at equivalents of pure operations.

+----------------------------------------------------------------------+
| Composing Values in a functor context                                |
+---------------------------------+----------+-------------+-----------+
|                                 | Functor  | Applicative | Monad     |
+================+================+==========+=============+===========+
| Lift           | Function App   | fmap     | fmap        | fmap      |
|                +----------------+----------+-------------+-----------+
|                | Value          |          | pure        | pure      |
+----------------+----------------+----------+-------------+-----------+
| Product                         |          | Apply       | Bind      |
| (Adjunction)                    |          |             |           |
+----------------+----------------+----------+-------------+-----------+
| Sum (Fold)     | Semigroup      |          | Alt (<|>)   | mplus     |
|                +----------------+----------+-------------+-----------+
|                | Monoid         |          | empty       | mzero     |
+----------------+----------------+----------+-------------+-----------+

Correspondences:

| N-ary application | Applicative
| Function chaining | Monad

Composing Types
---------------

Polymorphism is abstraction in the type space. Function is the basic
abstraction tool even in the type space. Type functions come in different
flavors:

* polymorphic type - explicit type function
* type families - pattern matched definition of type functions - partial
* parametrically polymorphic functions - functions of types, ultimately
  instantiated to a specific type
* typeclasses - ad-hoc polymorphism - functions are decides based on types -
  partial

References
----------

* http://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf Datatype-Generic Programming
* http://blog.functorial.com/posts/2015-12-06-Counterexamples.html Type class hierarchy in purescript
* http://www-kb.is.s.u-tokyo.ac.jp/~asada/papers/arrStrMnd.pdf Arrows are Strong Monads
