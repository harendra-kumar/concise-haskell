Composing Functors
==================

The indirection allows Functors to have a hidden track during composition.
The hidden track can be used for side effects among other things.

Summary
-------

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

Levels of Abstraction
---------------------

We have three different levels of programming abstraction i.e. functional,
applicative and monadic.

The essence of pure functional programming is functions. At the lowest level of
abstraction we write any computation as a composition of functions. Such a
program is always a single expression composed of functions. Though parts of
the expression can be defined as independent equations using let, where or top
level definitions. This is pure basic functional programming. The only way to
compose things in this method is by applying a function.

A regular function application has a `single track of composition`. It is a
pure composition because there is no scope of any side effect or state. However
a function wrapped in an applicative or monad allows a two track composition.
In addition to composing like pure functions, where a pure value is passed from
one function to another, they allow you to do something else as well i.e. they
can shoot from the hip too! The second track can do something behind the scenes
usually a side effect.

We then raise our level of abstraction to applicative programming. In
applicative form of programming we build upon the basic functional programming.
We can compose the basic functional building blocks using applicatives which
is just another form of composition where we give different semantics to
function application. In this form we can compose computations by, using a
lifted form of function application called applicative style. An applicative
program is again like a single expression but it consists of applicatives in
the expression. Here we added an additional way of composing different pieces
of the program called an applicative style of composition. This is achieved by
adding another level of abstraction using a Functor layer on top of the bare
types. The Functor layer allows us to add the applicative semantics.  The full
name of an applicative is applicative functor.

But applicative is not enough to express complicated dependencies between
computations. So we raise our abstraction a bit further and introduce a more
sophisticated way of composing computations called monadic composition. This is
a more flexible form of composition where we add another level of abstraction to
function composition, we don't just compose them by passing the result of one
to the other but by also doing something behind the scenes while we are doing
so. We can define independent computations and join them together using a
custom defined semantics which defines how to join those computations together,
what kind of dependencies do they have among each other. Monadic abstraction is
created by customizing the applicative functor to the semantics of monadic
composition.

More notes on functional, applicative and monadic programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The pure functional style is most pure without much abstraction and easy to
understand. Applicative by adding one more layer of abstraction requires us to
understand the semantics of that layer and makes a bit more difficult to
understand in comparison to pure functions. But the semantics are usually
intuitive, however one more thing to keep in mind. Monadic is much more powerful and
flexible and you can pretty much do anything under the hood so it is more
difficult to understand unless the semantics under the hood are intuitive. In
other words, pure functional style cannot be abused much, applicative has more
potential for abuse and monadic has highest potential for abuse or messing up.

In other words, pure functional is declarative (only let, where, equations are
used to break up a larger expression) functions are the only abstractions that you
need to understand and we know there cannot be any hidden effectful interaction
among components.

There are multiple ways to use functional style. The basic way is to get all the
arguments to one single function. Another way is to pass all the arguments in
records, we can also implement ways to only pass some of the arguments and keep
other optional. Another elegant way to use a functional interface is to use
builders. We can compose our input by multiple functions consuming their
arguments and returning a builder again.

Applicative is less declarative and in addition to abstractions via functions
we need to understand the applicative layer of abstraction, its added layer of
semantics. Monadic is imperative we need to understand how computations are
composed or dependent on each other.

It should be noted that monadic style may or may not use its full power but it
has the power. For example we can write pure code using monads or applicative
code using monads. Similarly we can write pure code using applicatives. The
point is that the more powerful style can potentially be difficult to
understand. This is similar to the fact that we can write pure code in
imperative languages by discipline. But there is always potential for abuse and
it is abused.

We can differentiate them based on where the magic is hidden. In functions the
magic is hidden just behind the functions. In applicatives and monads there is
another layer of indirection supplied by the Functor, allowing us to do some
magic in that layer.

In general, we should try to restrict ourselves to the pure functional style,
if that is not enough lift to the applicative style and if that is not powerful
enough then raise to the monadic style.

Classification
--------------

Each following item in this list is less general but more powerful than the preceding item.

The general principle that we have is that if we can combine two values then we
can combine any number of values. The two fundamental operations that we need
are "transform" and "combine". They come in different forms e.g. "pure" and
"apply" or "return" and "bind" or "arr" and "first" etc. Transform is
implication (i.e. the function type) and combine is sum or product.
There are two ways to combine two values, sum and product.

Free and non-free computations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a non-free way the collect and compose operations are intertwined whereas in
a free operation these are separated or modularised. A free structre separates
the pure combining logic from the pure data. The modularisation has a cost
though.

However, a free structure requires accumulation of the whole structure to see
the big picture, whereas a binary composition can work locally. Both have pros
and cons.

The Free structures allow you to construct the structure
first and then combine rather than doing both the steps at the same time. The
advantage of the free structures is "static analysis" i.e. you can look at the
whole structure before combining it, that allows you to make decisions based on
the big picture rather than only based on the limited information that you have
when you are combining two things.

A Free structure is pure data without the logic and you can add the logic
later. So a pure value is a free structure and a unary transformation on it is
the logic part.

Sum and Products
~~~~~~~~~~~~~~~~

A tuple (a,b) is a free "product" structure and a binary function contains the
user of the product for implication logic.  A tuple (a,a) or "Either a b", is a
free "sum" structure where you have the choice of using the first value or the
second , both are the same type or a replacement of each other. A binary
function contains the choice logic i.e. it can branch on any part of the sum
type based on sum logic. Note that a product and sum are different only in the
intended use for logic i.e.  whether you want to combine two items together or
you want to choose one of them. It is difference of combining or branching
(choice).

Therefore a product type can potentially consist of different types, whereas a
sum consists of the same type or replacements. Instead of saying the same type,
to make it more general, we can say the values which are intended as different
choices for the same abstract (logic) operation. Note that the product type can
also have the same types but they are intended to be used together rather than
individually in different parts of the logic. In other words, a product
provides the big picture (static analysis) whereas a sum provides only the
narrow picture of the given choice. A sum type condenses the data it can store
multiple possible choices in one place because we know at one time we are going
to select only one choice. Whereas a product type needs to store all of them
because we know the combining logic may need all of them at the same time
before it branches.

Sum types allow us to throw away the information that we do not need when we
made a certain choice. The big picture has a cost, it requires us to maintain
more information but allows more powerful logic.

The components of a product are all required at the same time. The components
of a sum type are required only one at a time.

A common operation on product types would be splitting and distributing, and a
common operation on sum types would be collecting and folding.

pure unary transformation.

+-----------------+-----------------------+-----------------+-----------------+
| Structure       | Structure Description | Logic Operation | Description     |
+=================+=======================+=================+=================+
| Pure value      | Unary value           | Unary Function  | a -> b          |
+-----------------+-----------------------+-----------------+-----------------+

Product types and cartesian (conjunctive) composition

+-----------------+-----------------------+-----------------+-----------------+
| Structure       | Structure Description | Logic Operation | Description     |
+=================+=======================+=================+=================+
| Tuple (a,b)     | binary product        | Binary Function | Pure binary     |
|                 |                       |                 | composition     |
|                 |                       |                 | (a, b) -> c     |
+-----------------+-----------------------+-----------------+-----------------+
| Curried binary Composition (a,b,c) -> d = (a,b) -> x ; (x,c) -> d           |
+-----------------+-----------------------+-----------------------------------+
| list [a]        | nary product          | Uncurried nary function app       |
+-----------------+-----------------------+-----------------------------------+
| n-tuple         | nary product of       | Uncurried nary function           |
| (a,b,c...)      | different types       | application                       |
+-----------------+-----------------------+-----------------------------------+

Coproduct types and monoidal (disjunctive) composition

+-----------------+------------------------------+----------------------------+
| Either a b      | Two way choice diff types    | Real sum type              |
+-----------------+------------------------------+----------------------------+
| Tuple (a,a)     | Two choices of the same type | product, can be used as sum|
+-----------------+------------------------------+----------------------------+
| list [a]        | n choices of the same type   | product, can be used as sum|
+-----------------+------------------------------+----------------------------+
| coproduct       | n choices of different types |                            |
+-----------------+------------------------------+----------------------------+

* http://hackage.haskell.org/package/oneOfN generalization of either

From functions to Monads
------------------------

+-----------------+-----------------------------------------------------------+
| Function with   | additional external/static/global inputs for the combine  |
| a closure       | operation                                                 |
+-----------------+-------------------------+---------------------------------+
| Composed funcs  | Categorical Composition | Combines functions in a sequence|
+-----------------+-------------------------+---------------------------------+
| Functor         | Contextual value, fmap, Contextual unary transformation   |
+-----------------+-----------------------------------------------------------+
| Applicative     | Contextual Sequence of values, Nary apply                 |
+-----------------+-----------------------------------------------------------+
| Alternative     | Contextual choices of values                              |
+-----------------+-----------------------------------------------------------+
| Arrow           | Compose tree of functions with additional static inputs   |
+-----------------+-----------------------------------------------------------+
| Monad           | Dependency tree of values                                 |
|                 | Embed computations between function applications in a ctxt|
|                 | combines a tree structure                                 |
|                 | Categorical composition like functions in Kliesli category|
+-----------------+-----------------------------------------------------------+

* There are things that arrows can do and monads cannot i.e. the static input.
* There are things that mondas can do but arrows cannot i.e. arrowapply.
* There are things that applicatives can do but monad cannot e.g. parallel
  application.

More types can have a functor instance than Applicatives. More types can have
an applicative instance than arrows. More types can have an arrow instance than
Monads.

Everything as Transformation and Continuation
---------------------------------------------

Transformation
~~~~~~~~~~~~~~

In what ways can we transform values? The general transformation operations
are:

* Unary transformation: a -> b

  * a -> a

* Binary transformartion (or composition) (a,b) -> c

  * a -> b -> c
  * (a,a) -> a -- special case when types are the same. monoidal folding

* Nary transformation:

  * built using binary transformation

    * a -> b -> c...-> d
    * a -> a -> a...-> a -- special case when types are the same
  * Free Nary transfomation

    * (a,b,c...) -> d
    * fold [a] -> a -- special case, folding a free structure using a binary op.
      Note that list is a free structure here and we are folding it using a separate
      "interpreter".

This shows that Monoidal composition is just a simpler, special case of
applicative composition where the types are the same. Also a free Monoidal
sequence is easier to represent than a free Applicative sequence since the
types are the same.  For applicative sequence we need a type-aligned data
structure. In a general applicative sequence we use an n-ary function to apply
whereas we can reduce a monoidal sequence by applying a binary function many
times.

The pure versions of the two kinds of transformations are "function
application" and "Monoid". The Functor versions are Applicative and Alternative
and then "Monad" and MonadPlus. Note that the monoid case is just a special
case of the more general function application case.

Functored Transformations
~~~~~~~~~~~~~~~~~~~~~~~~~

* Lifting the pure operations in a Functor

  * fmap puts a function inside a functor
  * Applicative applies an n-ary function to its arguments inside a functor
  * Free Applicative, use a separate structure and then apply at once
  * Alternative folds values inside a functor using a binary operation just like
    Monoids in the pure case.
  * Free Alternative, use a separate structure to hold values and then apply at
    once.

Continuations
~~~~~~~~~~~~~

In what ways can we combine the transformation of values? The composition of
transformation operations or we can call them continuations in general:

* Categorical composition is a way to combine the most basic form i.e. unary
  transformation. It is a special, least flexible, case of a general
  continuation.

  * ``(a -> b), (b -> c) => a -> c`` : (b -> c) is the continuation of (a -> b).
    We can call it a pure continuation. This is a useful special case of the
    more general cases described below. This is a "structured" way to compose
    rather than free form. Pass on value from one function to the next. This
    is a simple chain of functions, a one dimensional sequence.  We can
    combine them like Monoids using binary composition and the id function.
* ``x -> a, y -> b, (a,b) -> c`` : (x, y) -> c. Pass on values from many
  functions to the next.  This will form a tree of functions passing values
  forward. No static input is used. We have added the ability to compose
  "products" so another dimension got added, making this a tree rather than a
  simple chain.
* x -> a, b (static input), (a,b) -> c. This will form a tree of functions
  passing values forward, but also allowing use of static input.
* ...and so on. In general, there can be many ways in which different types of
  functions can be combined. N-ary functions (continuations) can take inputs
  from n different sources.

Functored Continuations
~~~~~~~~~~~~~~~~~~~~~~~

The next level is "pure function continuations" abstracted via a Functor.

* Arrows lift the composition of functions into a functor.
* A strong profunctor is equivalent to Arrow

Functored Transformation and Continuation: Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A Monad is the most powerful construct.

A monad combines the power of applicative and arrows (without static input)
i.e. it allows the most powerful ways of combining Functored values. It knows
function application (products) as well as continuation.

* (a -> m b), (b -> m c) => a -> m c
* (x -> m a), (y -> m b), (a,b) -> m c => (x, y) -> m c

It forms a tree of functions composed together. A Free monad has just the tree
of data and then we can apply the functions later i.e. fold the tree using the
appropriate continuations.

Generalising a Monoid
---------------------

The monoidal composition does not apply to heterogeneous type combiners because
it is a way to combine homogeneous types. So it does not apply to function
application, applicatives or the apply aspect of a Monad. However it applies to
function composition, arrows, alternative and monads.

Pure Monoids
~~~~~~~~~~~~

The most basic "homogeneous" (sum or choice - a sum type is multiple values of
the same type) type combiner is the semigroup append <> operator or a monoid
that appends pure values. The typeclass knows how to append any two values.
There is no concept of success or failure at a given step since the values are
pure and there is no second track (side effect track) to indicate a failure.
Think about the Maybe type for example, it combines the just values,
Nothing has no impact on the other value.

Similalrly at the function composition level we can combine pure functions
using a monoid. However pure functions do not have a side-track so there is no
failure.

Using a Monoid in an Effectful Composition
------------------------------------------

In effectful compositions we have two tracks a regular composition and a
side-track composition. On the side-track we can use a Monoidal composition. We
can choose a pure monoid and use its behavior on the side-track. For example we
can use Maybe or Either on the side-track.

In effectful computations we combine step-by-step and at each step there can be
an effect (the side track) that we combine using a Monoidal composition. We can
use the identity of the Monoid to indicate a terminal condition i.e. failure or
success. We can use the terminal condition to terminate the effectful
composition at that step.

We can combine arrows using a Maybe monoid behavior on the side-track and
terminate the function composition if some step returns Nothing.

We can combine an Alternative using a Maybe monoid behavior on the side track
and terminate the composition on failure and combine the results on success.

A monad in addition to applying (like applicative) also composes continuations
like arrows (the join operation is a monoidal operation). Using a Maybe Monoid
behavior we can terminate the Monad on failure and combine the results on
success. For example ExceptT has the Either behavior on the effect track.

Performing N tasks in a sequence
--------------------------------

Binary vs Nary operations for the N tasks. There is an option to fold the tasks
using a binary operation or an n-ary operation i.e. an operation that takes all
of them at once and then combine them.

Binary operations allow convenience to the programmer. Programmer does not have
to build a data structure and then call a function on that. Instead always use
a binary operation even to fold n tasks. it is simpler. We can use local state
passing to acheive some sort of limited batching combining only two adjacent
steps. The context passing in streamly is an example. The same concept is used
in the foldl library.

However, N-ary operations can be more efficient. It affords you the full big
picture across all the tasks. You can batch randomly i.e. shuffle and batch the
tasks.

Summary: Free structures
------------------------

+------------+----------------------------------------------------------------+
| Singleton  | pure type                                                      |
+------------+----------------------------------------------------------------+
| Tuple      | pure type, tuple, either, list                                 |
|            | conjunctive or disjunctive composition via functions           |
+------------+----------------------------------------------------------------+

+------------+----------------------------------------------------------------+
| Singleton? | Free Functor                                                   |
+------------+----------------------------------------------------------------+
| List       | Free Applicative/Alternative                                   |
+------------+----------------------------------------------------------------+
| Tree       | Free Monad                                                     |
+------------+----------------------------------------------------------------+

Consumers and Producers
-----------------------

In an effectful (Functor) world we use the term `action` instead of a function
for something that can consume or produce other values.  An action can have a
side-effect in addition to a pure output. Therefore it is possible for an
action to produce output wihtout an input or to consume input without an output
which is not possible in a pure function world. In such cases we use a dummy
type as a pure input/output type. Therefore as a pure function, a function
still necessarily has an input as well as output. TBD depict with pictures.

+---------+---------+-----------------------------+
| Consume | Produce | Object                      |
+=========+=========+=============================+
| N       | N       | Non-function value          |
|         |         | (Consumable)                |
+---------+---------+-----------------------------+
| N       | Y       | Source/Producer             |
+---------+---------+-----------------------------+
| Y       | N       | Sink/Consumer               |
+---------+---------+-----------------------------+
| Y       | Y       | Pipe, Producer and Consumer |
+---------+---------+-----------------------------+

Computational Products and Sums
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you think in computational terms then a product type is like nested "for"
loops, i.e. X x Y is "for each X each Y". This analogy will be more useful when
we take the concept of product to higher levels (computational) of abstraction.
A sum type is like sequential or appended statements i.e. "after X do Y".

+---------------+------------------------------+------------------------------+
| Type          | Data Equivalent              | Computational Equivalent     |
+===============+==============================+==============================+
| Product       | Multiplication               | Nesting                      |
+---------------+------------------------------+------------------------------+
| Sum           | Addition, Union or Appending | Sequencing                   |
+---------------+------------------------------+------------------------------+

Higher order Transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A combination of type level transformation (Functor) and value level
transformation (function application). The type level information provides a
fixed structure to data and the value level information provides the logic to
manipulate, compose or fold those structrues.

* Functor + Apply = Applicative Functors (pure, apply (<*>))
* Applicative + Monoid (sum) = Alternative (empty, choice (<|>))
* Applicative + Monoid (product) = Monad (return, bind (>>=))

* Monad

  * Monad + Alternative + Monoid (sum) = MonadPlus

  * MonadFix

* Comonad

* Category

  * Arrow

    * ArrowZero

      * ArrowZero + Monoid = ArroPlus

    * Arrow + Apply = ArrowApply

    * ArrowChoice

    * ArrowLoop

* ArrowApply <=> Monad

Monads
------

Do Expression
~~~~~~~~~~~~~

* TBD
* desugaring
* let in a do block
* where in a do block - cannot refer to bindings extracted from a monad

+-----------------------------------------------------------------------------+
| Multiline expressions in do syntax must be indented beyond the variable name|
+------------------------------------+----------------------------------------+
| Correct                            | Wrong                                  |
+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  main = do                         |  main = do                             |
|    let foo = case 0 of             |    let foo = case 0 of                 |
|         0 -> 4                     |        0 -> 4                          |
|    return ()                       |    return ()                           |
+------------------------------------+----------------------------------------+

Type Inference
--------------

* explain type inference for the programmer in monadic composition

CoAlternative
-------------

Have a "full" instead of empty.

::

  -- Because of the type of 'from', 'Coaplicative' must correspond to
  -- "non-empty containers", for the usual hand-wavy definition of "container".
  class Functor f => Coapplicative f where
    from :: f a -> a
    separate :: f (Either a b) -> Either (f a) (f b)

  instance Coapplicative Identity where
    from (Identity a) = a
    separate (Identity (Left a))  = Left  (Identity a)
    separate (Identity (Right b)) = Right (Identity b)

  -- Silly Haskell having no nontrivial comonoids.
  class Coapplicative f => Coalternative f where
    full :: f a -> ()
    full _ = ()

    split :: f a -> f (a, a)
    split = fmap (\a -> (a,a))

Free Composition
----------------

Most of the time haskell uses binary composition which is the simplest and most
reusable thing.
However we also need real n-ary operations that make the big picture visible
and not just two elements like in the binary operations. For example to run n
tasks in parallel combining them in binary manner could be very inefficient, we
need combine them all to a single output channel to reduce communication.
Similarly we need n-ary applicatives as well.

A free construction is like a buffered processing rather than a streamed
processing. We collect and then combine rather than keep combining as we
collect. A buffered processing allows us to do interesting transformations on
the collection before combining. This is important in many cases just buffering
and batching is important in many real-world use-cases for efficiency.

A lot of arguments about free-monad and the use of other free-structures vs
using the type-classes is misplaced. free-monads are to be used to improve the
speed of the algorithm rather than the speed of the program. They are supposed
to work at a higher level rather than at a lower level for each and everything.
For example if your algorithm requires batching of requests before you can
process them then you use a free construction. This is exactly the way you use
buffering, caching or batching in an application.

Many high-performance systems applications will be just dead if there is no
buffering, it is a very important concept. And we need a composable abstraction
for it which is a free construction of an abstract composition.

Also if the buffer becomes too large you have a problem because the processing
becomes expensive and can be very bursty. The same way free monads when
constructed in an unlimited manner can be very bad for performance. Most of the
solutions to the problem sound like "optimizing the buffering data structure"
which is good but the overarching question is do we need unlimited buffers for
our problem?

Free Structures
---------------

The free structures are simpler to explain and perhaps should be introduced
before we introduce the non-free ones. Free structures are folded using a
binary operation, the fold operation and the data are cleanly separate, i.e. we
first combine and then transform. In a non-free structure the fold logic and
the data are entangled together.

Applicative is a fold of sequences.
Monad/Comonad are folds of trees.

Kan Extensions
~~~~~~~~~~~~~~

::

  -- Right Kan Extension
  newtype Ran g h a = Ran (forall b. (a -> g b) -> h b)

  -- Left Kan Extension
  data Lan g h a = Lan (forall b. (g b -> a) (h b))

* http://comonad.com/reader/2008/kan-extensions/

Codensity
~~~~~~~~~

A special case of right Kan Extension where g and h are the same::

  newtype Codensity m a = Codensity (forall b. (a -> m b) -> m b)

* Reference: Asymptotic Improvement of Computations over Free Monads

Yoneda
~~~~~~

::

  type Yoneda = Ran Identity
  newtype Yoneda m a = Yoneda (forall b. (a -> b) -> m b)

* http://blog.sigfpe.com/2006/11/yoneda-lemma.html
* http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf When is one
  thing equal to some other thing?


References
----------

* https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Compose.html
* http://stackoverflow.com/questions/18024924/haskell-why-is-a-multi-line-let-expression-a-syntax-error
* https://markkarpov.com/post/free-monad-considered-harmful.html
