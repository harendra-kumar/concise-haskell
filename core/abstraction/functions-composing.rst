+------------------------+----------------------------------------------------+
| First order function   | None of the arguments is a function                |
+------------------------+----------------------------------------------------+
| Higher order functions | One or more argument is a function                 |
+------------------------+----------------------------------------------------+

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A higher arity function produces a function as output and a higher order
function accepts a function as input.

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

Operations on Functions
-----------------------

In this section we will look at ways to combine functions and values together.
There are three fundamental ways to combine functions and values:

* `Composition`: When the input type of a function matches the output type of
  another function, the two functions can be chained together by feeding the
  output of the latter to the input of the former::

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

* `Composition`:: Composing functions where the input of one of them is a
  function (higher order function)::

     -- the order of the combined function is at most max (n1, n2)

     f :: a -> (b -> c)
     g :: (b -> c) -> d

     k :: a -> d
     k x = g (f x)

* `Application` or `Currying`: A value matching one of the inputs of a function
  can be fed to the function to generate a lower order function or a data
  value::

    -- reduces the arity
    f :: a -> b -> c
    x :: a
    f x :: b -> c

    f :: (a -> b) -> c
    x :: b
    g :: b -> c
    g x = f (\_ -> x)

* `Extension`: Like an application reduces the arity, an extension increase the
  order of a function. A function and a value can be used such that the input
  of the function is modified to accept a function whose output matches the
  input of original function::

     -- increases the order
     f :: a -> b
     x :: c
     g :: (c -> a) -> b
     g k = f (k x)

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

Positive and Negative Positions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is easier to understand this by using a positive and negative position
terminology. What a function consumes (consumable) is negative position and
what it produces (product) is positive position (mnemonic - produce and positive
both start with p). Now, (a -> b) is in negative position in f and a is in
negative position in 'a -> b', it follows a multiplication rule and ``negative
x negative`` becomes positive, therefore `a` is in positive position in `f`.
Similarly, `b` is in negative position in `f` and is therefore consumed by `f`.

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


