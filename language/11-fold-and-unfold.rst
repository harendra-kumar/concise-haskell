Fold and Unfold
===============

.. contents:: Table of Contents
   :depth: 1

.. sectnum::

Recursive Algebraic Data Types
------------------------------

Lists
~~~~~

Lists are a result of pure combining of data without logic.

+----------+----------------------------------+-------------------------------+
| Type     | Values                           | Description                   |
+==========+==========+==========+============+===============================+
| [a]      | []       | 1 : []   | 1 : 2 : [] | List of Int                   |
|          |          |          |            | Explicit constructor syntax   |
|          +----------+----------+------------+-------------------------------+
|          | []       | [1]      | [1,2]      | Sugared syntax                |
|          +----------+----------+------------+-------------------------------+
|          | []       | ['a']    | ['a','b']  | List of chars (String)        |
|          +----------+----------+------------+-------------------------------+
|          | ""       | "a"      | "ab"       | String literals               |
+----------+----------+----------+------------+-------------------------------+

::

  data []   a = []    | :    a (List a)                -- Recursive

Note that Haskell's built-in list is not really a special syntax it is a user
defined data type, '[]' is the empty list constructor and ':' is the Cons
constructor. Though there is a syntactic sugar to specify lists in a more
convenient way [1, 2] is equivalent to 1 : 2 : [].

* List comprehensions
* See prelude for list functions

Recursive Binary Folding
------------------------

For combining arbitrary number of objects of the same type we can use a binary
product composition function recursively.

Combining Data
~~~~~~~~~~~~~~

Combining data requires assistance of a function because combining operation is
after all a dynamic transformation. To combine data we require imposition of a
certain abstract static structure on the data and an associated function to
assist combining that structure.

A binary operation can express a generalized n-ary operation.

Composing a set of values into a single value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assume that we have a collection of values of the same type, for example a list
of integers [1, 2, 3, 4, 5]. If we have a binary operation `f :: a -> a -> a`

Composing Two or More Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we have a collection of two or more values we can just use the binary
operation to compose them.

Folding
~~~~~~~

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

Folding or Sum Composition
~~~~~~~~~~~~~~~~~~~~~~~~~~

A special and interesting case of composition occurs when all the objects to be
combined are of the same type.  Just like a sum type represents many choices,
here the many objects of the same type represent different choices from the
same type. We combine them all into an output type.  We call such a composition
a fold or sum.

::
  A, A, A, ...

Using a binary product composition constructor or function we can fold any
number of objects of the same type recursively. For recursion we necessarily
need the output of the transformation to be the same as one of its inputs so
that we can feed back the output into the input.

C :: A -> B -> B

Semigroup Composition
~~~~~~~~~~~~~~~~~~~~~

When the objects being combined are of different types the only way to compose
is using a funciton or constructor. But when the objects being combined are of
the same type we have a convenient special case of general composition.  We can
now use a binary composition function to combine arbitrary number of objects.

Enables folding of two or more objects of the same type.
A semigroup can combine two or more objects using a binary operation. A
semigroup is a non-empty container since there is no way to represent an empty
value. A semigroup can be made a Monoid by using Option as a wrapper around it.

Monoidal Composition
~~~~~~~~~~~~~~~~~~~~

When the type has an identity we can have a more general composition where we
can combine 0 to n number of objects in the same way.

::

  mempty   :: a
  mappend  :: a -> a -> a
  you can fold t a : t a -> a

  Generalized:
    initial :: b
    combine :: a -> b -> b
  you can fold t a : t a -> b

Enables folding of 0 or more objects of the same type.
A monoid adds the concept of empty to the semigroup. It is a convenience over
semigroup with a built-in representation of the absence of a value (mempty).
That is we do not need an Option wrapper for that.

Some types have an in-built representation of mempty and therefore a semigroup
+ Option will not work for them e.g. integers with sum operation have 0 as an
empty value. They are natural monoids.

A monoid is useful where the concept of empty or absence of a value is
important. Just like Maybe. For example as a sink where we want to start empty
and collect 0 or more objects. A stream may yield 0 or more objects, collecting
and folding a stream requires a monoid unless we have an initial object to fold
with. A monoid is therefore useful in more cases because it can be used where a
semigroup can be used unless we specifically want to preclude the empty state.

The same code that requires two objects to combine can work with just one
object by supplying the other one as empty. This simplifies code over
semigroup.

* Semigroup | Data that can be combined | minimum two objects, enables operations on containers of objects

  * Monoid

    * Foldable

Basic Folds
-----------

fold:

Given a list container ``[1,2,3,4,5]``. There are two ways to compute the sum
of its elements::

  sum s (x : xs) = x + sum s xs    -- 1 + (2 + (3 + (4 + 5))) right associative
  sum s (x : xs) = sum (s + x) xs  -- (((1 + 2) + 3) + 4) + 5 left associative

The right associative version is called the right fold and the left associative
version is called the left fold. Note in foldr the fold operation is at the top
of the expression and recursion occurs as part of it. In foldl recursive call
is at the top level of the expression and the fold operation occurs as part of
it.

Now, the behavior of these operations depends on the evaluation strategy. When
the operation (+) is strict the right fold puts the whole structure on the
stack and unravels it in the end, whereas the left fold does not use the stack
at all. Note that when the container is strict we have already consumed space
in the container and the left fold does not require more space, on the other
hand when the container is lazy it is not using any space but we need that
space at the end when folding so they both are equivalent in that sense. They
are just duals of each other.

On the other hand when the operation is lazy in the second argument, the right
fold can keep yielding the next element from the container whereas the left
fold keeps building a lazy structure until the whole container is consumed,
consuming space proportional to the container size.  Note that in this example
we are using the (+) operation which is trivially strict.  However a folding
operation can be a whole pipeline of lazy operations.

To conclude, when consuming a structure (strict or lazy) using a pipeline of
lazy evaluation right fold is most efficient. For consuming a structure (strict
or lazy) using strict evaluation, left fold is most efficient. Combining a
strict operation on a large structure with a right fold or a lazy operation on
a large structure with a left fold may not be a good idea, it may not scale.
When we need to do that dividing up the structure in chunks and then folding is
a good strategy.

Lazy right fold = good - pull - infinite structures ok
Strict left fold = good - push - infinite structures ok
Strict right fold = bad - structure must be finite -- consumes the whole structure strictly, accumulating it on the stack.
Lazy left fold = bad - structure must be finite -- builds a lazy structure

Note that IO monad is strict. So to finally consume the output or input it is
inevitable to face the combination of strict evaluation and lazy structures.
However if the IO does not need to accumulate we can have a full lazy pipeline,
consuming one input and producing one output at a time (or chunks). However if
we use IO monad in them middle of a computation it cannot scale unless we use a
limited buffer.


foldr:

The following equations hold for a list::

  foldr (:) [] xs == xs
  map f = foldr ((:) . f) []

A lazy right fold can be equated with a pull style operation where the consumer
keeps pulling the next element from the container on-demand.


foldl:

Note that the ``identity`` is folded with the first element, therefore the
following reverses the list::

  reverse xs = foldl (flip (:)) []

A strict left fold is a push style mechanism where the producer keeps pushing
the next element to the consumer.

Mnemonics:

fold: remember "fuze it" - first argument is the fold function (fu), the second
is zero (ze), and ``it`` refers to the container we are folding.

fuze it : fold (fu)unction ze(ro) it

Argument order: The fold function in foldr takes the element first whereas in
foldl it takes the list first which is in accordance with their behavior of
reducing the element first or the list first.

foldl makes sense in general for left-associative operators, and foldr makes
sense for right-associative operators.  Left-associative operators must have
the type (a -> b -> a) i.e. result type is same as left argument e.g. ``(m >>=
f1) >>= f2``, while right-associative operators must have type (a -> b -> b)
i.e. result type is the same as the right argument e.g. ``1 : (2 : [])``.

+-----------------------------------------------------------------------------+
| Folds                                                                       |
+----------------------+------------------------------------------------------+
| fold a container of  | fold :: Monoid m => t m -> m                         |
| monoids              |                                                      |
+----------------------+------------------------------------------------------+
| map elements to      | foldMap :: Monoid m => (a -> m) -> t a -> m          |
| monoids then fold    |                                                      |
+----------------------+------------------------------------------------------+
| Right fold           | foldr :: (a -> b -> b) -> b -> t a -> b              |
+----------------------+------------------------------------------------------+
| Left fold            | foldl :: (b -> a -> b) -> b -> t a -> b              |
+----------------------+------------------------------------------------------+
| fold a nonempty      | foldr1/foldl1 :: (a -> a -> a) -> t a -> a           |
| container            |                                                      |
+----------------------+------------------------------------------------------+

Construction Using Folds
~~~~~~~~~~~~~~~~~~~~~~~~

When we fold using a constructor:

* A list is just a binary recursive composition of the same type using a
  constructor.
* A tree is a binary recursive composition of two different types
  using a constructor.

Foldable
--------

Foldable   -- values folded as pure data (does not require functor instance)

Typeclass Functions
~~~~~~~~~~~~~~~~~~~

::

  fold $ map Sum [1,2,3]
  foldMap Sum [1,2,3]

+--------+------+--------+------+---------+---------+-----+---------+
| toList | null | length | elem | maximum | minimum | sum | product |
+--------+------+--------+------+---------+---------+-----+---------+

Other Functions
~~~~~~~~~~~~~~~

+---------+-----------+-----+----+-----+-----+-----------+-----------+
| concat  | concatMap | and | or | any | all | maximumBy | minimumBy |
+---------+-----------+-----+----+-----+-----+-----------+-----------+

+---------+-----------+
| notElem | find      |
+---------+-----------+

Unfolds
-------

* https://hackage.haskell.org/package/unfoldable

  unfolds are enumerations?

Comonoid
~~~~~~~~

A comonoid in a monoidal category is a monoid in the dual category, what is
the problem?

The dual of Monoid. The way a Monoid (e.g. writer) accumulates a comonoid
duplicates. Like a monoid has mempty a comonoid would have a "mfull".  A monoid
keeps adding stuff to empty. A comonoid would keep distributing stuff from
"mfull". For example a copy constructor of an object in C++ duplicates itself,
it is a comonoid.

See how Comonoid relates to a Comonad the way a Monoid relates to a Monad.

People confuse a comonoid with reader monad. Comonoid relates to distributive
the same way monoid relates to traversable. Every Distributive Functor is
actually Representable. Representable endofunctors over the category of Haskell
types are isomorphic to the reader monad.

References
----------

* https://en.wikipedia.org/wiki/Fold_(higher-order_function)
* https://groups.google.com/forum/#!topic/elm-discuss/ehsV6-YveFA fold function argument order
* http://www.cs.nott.ac.uk/~pszgmh/fold.pdf A tutorial on the universality and
  expressiveness of fold
* http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire
* https://wiki.haskell.org/Attribute_grammar Traversal & folds using attribute grammar

Streaming folds
~~~~~~~~~~~~~~~

* http://squing.blogspot.in/2008/11/beautiful-folding.html
* http://conal.net/blog/posts/another-lovely-example-of-type-class-morphisms
* http://conal.net/blog/posts/more-beautiful-fold-zipping
* http://www.haskellforall.com/2013/08/composable-streaming-folds.html
* https://www.schoolofhaskell.com/user/edwardk/cellular-automata/part-2

Category theory
~~~~~~~~~~~~~~~

* https://wiki.haskell.org/Catamorphisms
* https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms
* https://ulissesaraujo.wordpress.com/2007/12/19/catamorphisms-in-haskell/

Comonoids
~~~~~~~~~

* https://stackoverflow.com/questions/23855070/what-does-a-nontrivial-comonoid-look-like
* https://stackoverflow.com/questions/15418075/the-reader-monad/15419213#15419213

Packages
--------

* http://hackage.haskell.org/package/monad-supply-0.3/docs/Control-Monad-Supply.html
* http://hackage.haskell.org/package/monoid-subclasses-0.1/docs/Data-Monoid-Factorial.html
* http://hackage.haskell.org/package/monoid-subclasses
