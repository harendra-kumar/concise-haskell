.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Composing Functions
===================

.. contents:: Table of Contents
   :depth: 1

.. sectnum::

Terminology
-----------

+------------------------+----------------------------------------------------+
| First order function   | None of the arguments is a function                |
+------------------------+----------------------------------------------------+
| Higher order functions | One or more argument is a function                 |
+------------------------+----------------------------------------------------+
| Positive position      | type variable in result type of a function         |
+------------------------+----------------------------------------------------+
| Negative position      | type variable in arguments of a function           |
+------------------------+----------------------------------------------------+

Functions as First Class Values
-------------------------------

Functions are first class values. Till now we have only used functions by
applying them to some values. However we can combine function together and
create new functions, without applying them to values. We can pass functions to
functions and return functions from functions. A function is like a pipe and we
can join many of them to create an aribtrarily complex network of pipelines.
This is very powerful and modular way to compose programs.

Functions as Transformations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function is a transformation that has one or more inputs and precisely one
output. However a multi-input function can be represented as a single input
function that produces a function consuming the rest of the inputs.
Therefore, fundamentally a function can be considered as a transformation with
precisely one input and one output.

Here is an example of a simple function that consumes ``a`` and produces ``b``.
Often, we also say that it is a consumer of ``a`` and producer of ``b``.

+----------+--------+-------+--------+--------+
| function |        | input |        | output |
+----------+--------+-------+--------+--------+
|  ``f``   | ``::`` | ``a`` | ``->`` | ``b``  |
+----------+--------+-------+--------+--------+

Note that the input as well as the output could be data or function.

Functions as Consumers and Producers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is very important to keep in mind the obvious fact that a function has two
sides viz.  input and output or consumer and producer. This aspect is very
important in classifying the abstractions that will come later.

A Haskell program is a pipeline built from function compositions. A pipeline
has pipes with consumer end and a producer end connected together. Functions
can be connected together in many different ways providing a tool to create a
complex data flow network.

Functions combined with algebraic data types are a very powerful tool but it
may be too powerful. Therefore we build many higher level abstractions or
reusable patterns on top of the basic building blocks i.e. functions to stay
organized and provide handy tools to solve problems.  These patterns allow us
to solve problems in a more structured manner. Before we talk about higher
level abstractions, let us first see how we can define, slice and dice
functions.

Higher Arity/Order Functions
----------------------------

A higher arity (n-ary i.e. not unary) function produces a function as output
and a higher order function accepts a function as input.

An n-ary function provides a way to the values of parameters to
in the function definition expression::

  -- multi-arity functions, producing a function as output
  -- nesting of functions on the output side
  -- we will refer to the rank as "arity", arity is 3 in this example
  f :: a -> (b -> (c -> d))

A higher order function provides a way to plug pipes (inputs and outputs)
in the function definition expression::

  -- accepting a function as input
  -- nesting of functions on the input side
  -- we will refer to the rank as "order", order is 3 in this example
  f :: ((a -> b) -> c) -> d

Note that ``->`` is right associative and therefore ``f :: a -> (b -> c)`` is
the same as ``f :: a -> b -> c``. However ``f :: (a -> b) -> c`` is entirely
different, it accepts one argument which is a function.

Categorical Composition
-----------------------

Combining pure (unary) transformations themselves:
semigroupoid, monoid (category) composition.

* `Direct Composition`: When the input type of a function matches the output
  type of another function, the two functions can be chained together by
  feeding the output of the latter to the input of the former::

    -- the arity of the composed function is at least n1 + n2 - 1
    -- output modification, same order, arity
    f :: a -> b
    g :: b -> c
    k :: a -> c
    k = f . g

     -- input modification, same order, arity
     f :: a -> b
     g :: c -> a
     k :: c -> b
     k = f . g

  This is the direct style or forward style composition. The output of a
  function is used by another function inside the caller of this function i.e.
  outside this function.

* `CPS Composition`:: Composing functions where the input of one of them is a
  function (higher order function)::

     -- the order of the combined function is at most max (n1, n2)

     f :: a -> (b -> c)
     g :: (b -> c) -> d

     k :: a -> d
     k x = g (f x)

  This is a CPS style or backward style composition. A function passes its
  output to another function inside this function i.e. the function to be
  invoked is one of the inputs to this function.

Function Applications (Currying)
--------------------------------

`Application` or `Currying`: A value matching one of the inputs of a function
can be fed to the function to generate a lower order function or a data value::

    -- reduces the arity
    f :: a -> b -> c
    x :: a
    f x :: b -> c

    f :: (a -> b) -> c
    x :: b
    g :: b -> c
    g x = f (\_ -> x)

Currying first order functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currying refers to function application in multi-arity functions.
Consider this function definition::

  f :: a -> b -> c -> d
  f :: a -> (b -> (c -> d))

We can supply any combination of arguments to this function and leave others
unsatisfied. For regular function currying the arguments must be fed in order,
if we need to curry arguments out of order then we need to make a new function
using a lambda or otherwise. Assume that we have values `x`, `y` and `z` in
scope to be used for parameters `a`, `b` and `c` respectively.

+-----------------+-----------------+-------------+---------------------------+
| consumed (-ve)  | produced (+ve)  | Curry       | Lambda                    |
+=================+=================+=============+===========================+
| a               | (b -> (c -> d)) | f x         | \b c -> f x b c           |
+-----------------+-----------------+-------------+---------------------------+
| b               | a -> c -> d     |             | \a c -> f a y c           |
+-----------------+-----------------+-------------+---------------------------+
| c               | a -> b -> d     |             | \a b -> f a b z           |
+-----------------+-----------------+-------------+---------------------------+
| a, b            | (c -> d)        | f x y       | \c -> f x y c             |
+-----------------+-----------------+-------------+---------------------------+
| b, c            | a -> d          |             | \a -> f a y z             |
+-----------------+-----------------+-------------+---------------------------+
| a, c            | b -> d          |             | \b -> f x b z             |
+-----------------+-----------------+-------------+---------------------------+
| a, b, c         | d               | f x y z     | f x y z                   |
+-----------------+-----------------+-------------+---------------------------+

In any of the productions positive or negative status of `a`, `b`, `c` & `d`
never changes. The regular function application provides us only three ways out
of the seven possible ways to consume inputs.

Currying Higher-order functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function which takes another function as an argument is a higher order
function.

Consider this function::

  f :: (a -> b) -> c
       g^^^^^^^                -- Positive position
        -                      -- Negative position

The function `a -> b` consumes an `a` and produces a `b`. `f` does direct
opposite, it produces that `a` and consumes the `b`. This reversal is
important to keep in mind and becomes even more important when we try to
understand higher order function with even deeper nesting. Every nesting level
flips the consumed or produced roles of the arguments of the function.

+---------------------------------+------------------------+
| Supplied by user, consumed by f | Supplied by f          |
+=================================+========================+
| g :: a -> b                     | x :: a                 |
+---------------------------------+------------------------+

Positive and Negative Positions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the context of higher order functions the positive and negative position
terminology is quite useful when we have a nesting deeper than one level. What
a function consumes (consumable) is negative position and what it produces
(product) is positive position (mnemonic - produce and positive both start with
p). As we will see later the sign of the position allows us to quickly
determine consumable or producer role of an argument by using multiplication of
signs.

Example: Two level nesting
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  f :: ((a -> b) -> c) -> d
       g^^^^^^^^^^^^^^           -- Positive position
        --------                 -- Negative position
         x                       -- Positive position

This function is fully applied by supplying two arguments, for example `f g x`.
To understand this it is useful to think in terms of which function is provided
by us and which function is supplied by f.

+---------------------------+------------------------+
| Consumed by f             | Supplied by f          |
+===========================+========================+
| g :: (a -> b) -> c        | k :: a -> b            |
+---------------------------+------------------------+
| x :: a                    |                        |
+---------------------------+------------------------+

We can curry the functions that are supplied by `f` by applying them partially
to the arguments that are supplied by us.

+------------------------+------------------------+---------------------------+
| input                  | Output                 | Example                   |
+========================+========================+===========================+
| g :: (a -> b) -> c     | a -> d                 | f g                       |
+------------------------+------------------------+---------------------------+
| x :: a                 | (b -> c) -> d          | \bc -> f (\k -> bc (k x)) |
+------------------------+------------------------+---------------------------+
| g :: (a -> b) -> c,    | d                      | f g x                     |
| x :: a                 |                        |                           |
+------------------------+------------------------+---------------------------+

See `liftBaseWith` and `defaultLiftWith` for real examples.

Example: Three level nesting
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now lets take an example of a function with three nesting levels.

::

  f :: (((a -> b) -> c) -> d) -> e
       g^^^^^^^^^^^^^^^^^^^^^               -- Positive position
        ---------------                     -- Negative position
         h^^^^^^^                           -- Positive
          -                                 -- Negative

This function is fully applied by supplying two arguments, for example `f g h`.

+---------------------------+------------------------+
| Consumed by f             | Supplied by f          |
+===========================+========================+
| g :: ((a -> b) -> c) -> d | k :: (a -> b) -> c     |
+---------------------------+------------------------+
| h :: a -> b               | x :: a                 |
+---------------------------+------------------------+

We can curry the functions that are supplied by `f` by applying them partially
to the arguments that are supplied by us.

+------------------------+------------------------+---------------------------+
| Consumed by f          | Supplied by f          | Example                   |
+========================+========================+===========================+
| ((a -> b) -> c) -> d   | (a -> b) -> e          | f g                       |
+------------------------+------------------------+---------------------------+
| a -> b                 | (c -> d) -> e          | \cd -> f (\k -> cd (k h)) |
+------------------------+------------------------+---------------------------+
| ((a -> b) -> c) -> d,  | e                      | f g h                     |
| a -> b                 |                        |                           |
+------------------------+------------------------+---------------------------+

Nesting with Currying
^^^^^^^^^^^^^^^^^^^^^

::

  f :: (((a -> b) -> c) -> d) -> m -> e -- f g x h
       g^^^^^^^^^^^^^^^^^^^^^    x
        ---------------
         h^^^^^^^
  f :: m -> (((a -> b) -> c) -> d) -> e -- f x g h

It is easier to explain this using the positive and negative positions
terminology. We can see that, (a -> b) is in negative position in f and a is in
negative position in 'a -> b', it follows a multiplication rule and ``negative
x negative`` becomes positive, therefore `a` is in positive position in `f`.
Similarly, `b` is in negative position in `f` and is therefore consumed by `f`.

Function Extensions
-------------------

`Extension`: Like an application reduces the arity, an extension increases the
order of a function. A function and a value can be used such that the input
of the function is modified to accept a function whose output matches the
input of original function::

     -- increases the order
     f :: a -> b
     x :: c
     g :: (c -> a) -> b
     g k = f (k x)

Extensions
~~~~~~~~~~

* XXX This section needs to be cleaned up.

Extensions are higher order functions.  A continuation is an interesting
extension.

::

  cont :: (a -> r) -> r

``a -> r`` is a missing piece in this computation which is supplied later. The
missing piece is what produces the final result.

A continuation has already decided the final result (``r``) type of the
computation, it also has an intermediate value ``a``. What it needs is a
function that cosumes the intermediate value and generates a result type which
may be consumed by ``cont`` to generate the final result. The continuation ``a
-> r`` is sort of sandwiched somewhere inside ``cont``.

From a CPS perspective, ($ 2) is a suspended computation: a function with
general type (a -> r) -> r which, given another function as argument, produces
a final result. The a -> r argument is the continuation; it specifies how the
computation will be brought to a conclusion.
Note that suspended computations are largely interchangeable with plain values:
flip ($) [1] converts any value into a suspended computation, and passing id as
its continuation gives back the original value.

When we apply a function, we say that the function consumes the value. However,
a function application is a complementing operation and we can flip the
perspective and say that the value is eaten by some function instead. ``flip
($)`` flips the value into a function which eats some function to complete the
application. Or we can say that we wrapped the value into a higher order match
maker function which has eaten one part of the match and is waiting for the
other part. Continuations create holes in a computation to be filled later, it
is an incomplete or suspended computation.

Continuation is just a dual of the function application. They are just another
way of composing - in the opposite direction. We just have to think from the
end to the beginning rather than the other way round.

You have f, you pass it a value, the value is - you have g and you pass x to
it::

  f (g x)

You have x, it is to be fed to someone (g) and that in turn is to be fed to
someone else (f)::

  \f -> f y
  \g -> g x

A continuation is a reverse function application style. In a continuation we
say that this value is to be used by someone, say k. In a forward application
style we say this function will be applied to some value.

https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style pythgoras
example.

In fact a continuation passing style is a more straightforward thinking. For
example::

  pythagoras_cps x y = \k ->
  square_cps x $ \x_squared ->
  square_cps y $ \y_squared -> -- square y and the pass the result to second arg
  add_cps x_squared y_squared $ k -- add two values and pass the result to k

Here we say, square x, then square y, then add them and then pass the result to
k. In contrast see the regular function application style::

  pythagoras x y = add (square x) (square y)

we are saying, add two things, first thing is a square of x, the second thing
is a square of y.

Both ways are equivalent, just a dual of each other. In continuation style a
value is provided and we need who eats it i.e. the continuation of this value.

The Cont monad makes composing the continuations much easier. Basically it
allows us to write the continuations in the straight application style::

  pythagoras_cont :: Int -> Int -> Cont r Int
  pythagoras_cont x y = do
      x_squared <- return (square x)  -- perform square of x, use it later
      y_squared <- return (square y)  -- perform square of y use it later
      return (x_squared + y_squared)  -- add the squares, use the result later

Cont monad straightens the callback style programming. A continuation can be
thought of as a callback. In a callback style "square x" can take a callback
and call it when it is done squaring x. In a continuation style the rest of the
computation is the callback or continuation of "square x" though written in a
straightforward manner because all the callbacks are lined up sequentially.

Event driven programming is suited to a cont monad. Event driven programming
and upfront available value driven programming are duals of each other. In
regular programming we have all the values available and compute using that. In
event driven programming values are generated by events and when it is
generated we need to pass it to the consumer, this is reverse style. In the
same way cont monad is a dual of the regular straightforward funciton
applicaiton style.

A more general, MachineT example::

  The CPS form is:

  newtype PlanT k o m a = PlanT
    { runPlanT :: forall r.
        (a -> m r) ->                                     -- Done a
        (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
        (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
        m r ->                                            -- Fail
        m r
    }

runPlanT is a computation that takes multiple missing pieces. The PlanT monad
allows us to compose a computation and then we can supply these missing pieces
later to complete the computation. The missing pieces are all continuations as
their result type is the same as the result type of the whole computation.

::

  runPlanT :: forall r. (a -> m r) -> (o -> m r -> m r) -> (forall z. (z -> m r)
  -> k z -> m r -> m r) -> m r -> m r

  The CPS form is equivalent to the following regular form:

  data Plan k o a
    = Done a              -- runPlanT supplies a to a -> m r
    | Yield o (Plan k o a) -- runPlanT supplies o and m r to (o -> m r -> m r)
    | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
    | Fail

+--------------------------------------+--------------------------------------+
| Direct Style                         | CPS style                            |
+--------------------------------------+--------------------------------------+
| Has a function needs a value         | Has a value needs a function         |
+--------------------------------------+--------------------------------------+
| Has "a -> r" needs an "a"            | Has an "a" needs an "a -> r"         |
+--------------------------------------+--------------------------------------+
| A direct monad composes functions    | In a CPS monad the input type is     |
| with the return value of the same    | decided by the function and the      |
| type i.e. b in (a -> m b)            | output type is decided by the cont   |
| All function return the same type    | i.e. r in "(a -> m r) -> m r" is free|
+--------------------------------------+--------------------------------------+
| Functions are fixed, arguments vary  | Arguments are fixed, functions vary  |
+--------------------------------------+--------------------------------------+
| Values go to functions               | functions go to values               |
+--------------------------------------+--------------------------------------+
| Values are passed around             | Functions are passed around.         |
+--------------------------------------+--------------------------------------+
| The type is a sum of values          | The type is a product of functions   |
| Stop | Yield a (Stream a)            | f :: m r -> (a -> m r -> m r) -> m r |
+--------------------------------------+--------------------------------------+
| Types/structure of values are fixed  | Types/structure of functions r fixed |
+--------------------------------------+--------------------------------------+

Direct Style
~~~~~~~~~~~~

::

  -- The data structure is represented by Constructors
  data Stream a = Stop | Yield a (Stream a)

  -- Generation - destruction, followed by construction of Stream
  -- We keep calling the constructors and finally when we stop we call the base
  -- constructor.
  unfoldr :: (b -> Maybe (a, b)) -> b -> Stream a
  unfoldr step seed =
    case step seed of
      Nothing -> Stop
      Just (a, b) -> Yield a (unfoldr step b)

  -- Consumption: destruction of Stream, followed by construction
  -- We pattern match the constructors and recurse until we reach the base case.
  length :: Stream a -> Int
  length s = go s 0
    where go s1 n =
      case s1 of
        Stop -> n
        Yield _ r = go r (n + 1)

CPS Style
~~~~~~~~~

::

  -- The data structure is represented by functions
  -- First argument is Stop, the second argument is Yield, r is the Stream and
  -- a is the element in the stream.
  {-# LANGUAGE RankNTypes #-}
  module Test where

  -- r represents the final data structure that we have to build. It will be
  -- decided by the actual continuations passed when we call runStream.
  data Stream a = Stream {runStream :: forall r. (r -> (a -> (Stream a) -> r) -> r)}

  -- We keep calling yield function and finally we call the stop function when
  -- reach the end. The stop and yield continuations are just like constructors,
  -- the Stop and Yield constructors in direct style, they are closures.
  -- Constructors and closures are in fact represented in the same way at
  -- implementation level.
  -- Finally when we process the stream we provide these continuation closures.
  unfoldr :: (b -> Maybe (a, b)) -> b -> Stream a
  unfoldr step seed = Stream $ \stop yield ->
      case step seed of
          Nothing -> stop
          Just (a, b) -> yield a (unfoldr step b)

  -- The consumer defines the continuations rather than calling them. The
  -- continuations stop and yield will be called when the stream is constructed
  -- e.g. by unfoldr.
  length :: Stream a -> Int
  length s = go s 0
    where go s1 n =
              let stop = n
                  yield _ r = go r (n + 1)
              in (runStream s) stop yield

  length $ unfoldr ...

Notice how the control flow works in imperative terms - the consumer (length)
supplies functions that are called directly by the producer (unfoldr) who is
building the data structure in the first place. So the data structure is not
actually built in memory it is directly consumed via a pipeline of functions.

The continuation is consumer, it is made available directly to the
producer, if and when it needs to call it.

CPS Style Variants
~~~~~~~~~~~~~~~~~~

`Consumer driven`: In the previous example we yielded one element and the rest
of the stream. The consumer consumes one element and then processes the rest of
the stream again recursively. That is the remaining stream is threaded around
in the recursive computation. The recursion is driven from the consumer side
i.e. yield will be called repeatedly by the consumer until the stream ends.

In this case it is easy to switch producers or multiplex the producers. But its
not easy to multiplex the consumers.  We pull remotely from the producer and
have the processing logic near where we build, that's why the pipeline has
visibility of multiple producers but only one consumer.


`Producer driven`: There is a dual of this model where instead of yielding the
rest of the stream we yield an element and the output built till now. That is
the accumulated output is threaded around in the recursive computation. The
recursion is driven from the producer side i.e. "build" is called repeatedly by
the producer until the stream ends.
For example::

  newtype Stream m a =
      Stream {
          runStream :: forall r.
                 Maybe (SVar m a)           -- local state
              -> r                          -- accumulator
              -> (a -> r -> ExceptT r m r)  -- build
              -> ExceptT r m r
      }

In this case it is easy to switch or multiplex the consumers. But its not easy
to multiplex the producers.  We fold at the producer and push the output to the
remote consumer, that why the pipeline has the visibility of multiple consumers
but only one producer.

Advanced Example
~~~~~~~~~~~~~~~~

See simple-conduit package. It uses the producer driven recursion.

Direct vs CPS Style Performance Considerations
----------------------------------------------

Constructors (or data) and functions are duals of each other and any practical
program needs both of them to perform a task.  The same program can be
expressed in two dual ways.  A Haskell program can be thought of as a pipeline
with multiple stages. Each stage couples with the next stage to pass on its
output.  The coupling between two stages can either be a function or data
(constructor).  Its like whther you want to wear your shirt outside in or
inside out.

In a direct style the programmer models the task using constructors (data)
first, when executing, we examine the constructors, make decisions based on
them and then may call some functions to perform a task, which may use
constructors to build data for the next stage and pass it on to the next stage.
We start with data and end up building data for the next stage. The model is
Data-Function-Data. The cupling between two stages is data, we pass around data
(constructors).

In CPS style, the programmer models the task using functions first, a function
would process some data, may use constructors to build data and then build
another function (continuation) to be called for the next stage of processing
and the execution proceeds like that. We start with a function and we end up
with building another function. The model is Function-Data-Function. The
coupling between two stages is a function we pass around functions (closures).

Both these models are duals of each other and in an ideal world both should
work equally well. However the devil is in the operational details. Because of
the compiler implementation, machine execution models one way may work better
than the other for a given program. To understand them better let's take a
closer look at both.

Operational Mechanisms
~~~~~~~~~~~~~~~~~~~~~~

In operational terms pure constructors are allocated from the heap and pure
functions use the stack to perform a function call.

Direct Style:
Constructors        Function calls/Recursion
Heap                Stack (quite a bit)

CPS Style:
Continuation (function) call    Constructors/Closures (unfolded recursion)
Stack (very little)             Heap

Direct Style Cost Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~

In direct style we construct the input as data using data constructors and pass
it to a function. The function examines the constructors, transforms them and
then builds more data for the next stage. This process involves dismantling
input data which gets garbage collected and allocating new output data from the
heap. So there is a garbage collection cost involved at each stage. However it
is possible that the compiler optimizations cause the data to be passed on from
one stage to another without actually allocating it from the heap i.e. using
the registers when possible. The intermediate step to allocate and release may
be eliminated.

When we pass on the output to the function for the next stage there is a
function call cost involved. If there is a register spill we may have to pass
the arguments on the stack. If the function needs to return we may need to save
the state of the current function on the stack. More the number of variables
passed on to the next stages the more will be the cost of a funciton call. The
longer we hold on to the data the more will be the cost of keeping it in the
heap and saving the pointers on the stack across function calls. Large number
of machine registers can help in passing around the variables more efficiently.

Function inlining can flatten out the intermediate functions and reduce the
cost of function calls.

So the direct model translates to calling a function that needs to return back
with some data. This means use of heap for the data and use of stack for the
function calls to save the context since they need to return back to the
current context.

CPS Style Cost Analysis
~~~~~~~~~~~~~~~~~~~~~~~

In the CPS style, there is no state to save when we call the next function
(continuation) as we are never going to return. We pass functions to the next
stage function. As long as the number of functions we are passing is not many,
the function call is cheap as there is less spill, no state to save on the
stack. The main cost involved is in building the closures that we pass around
to the next stage. The closures contain all the state explicitly and involve
allocations from the heap. However, these closures/function calls cannot be
inlined by compiler. The more the number of stages, the more will be the cost,
this is only in the hands of the programmer and not the compiler. The heap
allocations to make the closures is fixed since the structure of the closures
is fixed. The only optimizations left are within a single stage.

Pure Code
~~~~~~~~~

Monadic Code
~~~~~~~~~~~~

Selection Functions
-------------------

Selection is a dual of continuation.

A function is a map from inputs to outputs. A selection function selects one of
the many possible inputs of a function. Instead of taking an input and
producing an output, it takes a function and produces the input that the
function allows to take.

::

  data Sel r x = Sel {runSel :: (x → r) → x}

A continuation runs forward, whereas a selection works backwards.

References
----------

* https://arxiv.org/pdf/1406.2058.pdf Monad Transformers for Backtracking Search
* http://math.andrej.com/2008/11/21/a-haskell-monad-for-infinite-search-in-finite-time/ A Haskell monad for infinite search in finite time
* https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf Selection Functions, Bar Recursion, and Backward Induction
* https://stackoverflow.com/questions/42378073/how-to-use-the-select-monad-to-solve-n-queens
* https://ebooks.au.dk/index.php/aul/catalog/download/4/4/26-1?inline=1 The selection monad as a CPS translation
