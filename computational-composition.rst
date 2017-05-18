Terminology
-----------

+-------------------+---------------------------------------------------------+
| Pure              | A pure value or pure type is used to refer to types     |
|                   | which are not wrapped in a monad type.                  |
+-------------------+---------------------------------------------------------+

* TBD - define actions, effects, effectful computation etc.
* Three types of values, abstract value (with holes), pure concrete value
  (neutral), effects (produce a side effect).

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
builders. We can compose out input by multiple functions consuming their
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

Composition of Functions
------------------------

Keep in mind that applicatives, monads and arrow  types always contain
functions or actions. They compose functions.

::

  -- function
  f :: a -> b

  -- composition
  (>>>) :: (a -> b) -> (b -> c) -> (a -> c)

  -- map a function
  fmap :: (a -> b) -> (f a -> f b)

  -- to be able to use the applicative style composition we need to be able to
  -- inject values inside an applicative functor
  pure :: a -> f a

  -- apply a function inside a functor
  <*> :: f (a -> b) -> (f a -> f b)

  show ((+ 1) 5)
  Identity show <*> (Identity (+ 1) <*> Identity 5)

  -- to be able to use the monadic style composition we need to be able to
  -- inject values inside a monadic functor
  return :: a -> f a

  -- compose functions inside a monadic functor
  -- Kleisli composition
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

  show . (+1) $ 5
  return . show <=< return . (+1) $ 5 :: Identity String

  -- Applicative functor is a special case of monadic functor
  ap :: (Monad m) => m (a -> b) -> m a -> m b
  ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

  =<< :: (a -> f b) -> (f a -> f b)
  >>= :: f a -> (a -> f b) -> f b

  return :: a -> f a
  fmap   :: (a -> b)   -> (f a -> f b)
  <*>    :: f (a -> b) -> (f a -> f b)
  =<<    :: (a -> f b) -> (f a -> f b)

Applicative
-----------

Applicative functors are functors for which there is also a natural
transformation that preserve monoidal structure of their source/target
categories. In the case of Haskell's Applicative endofunctors (because their
source and target categories is Hask), the monoidal structure is the Cartesian
product. So for an Applicative functor there are natural transformations φ: (f
a, f b) -> f (a, b) and ι: () -> f ()

Also called "strong lax monoidal functor". The monoidal formulation is
more elegant. Apply a function (functor property) and combine (monoidal
property)::

  class (Functor f) => Applicative f where
    pure :: a -> f a
    zip :: (f a, f b) -> f (a, b)

A functor type allows you to have function objects wrapped in that type,
but it does not know how to apply them to values wrapped in the same
type. Applicative adds that via <*>. An applicative type provides a type
specific way of applying functions contained in that type to values
contained in that same type::

  <*> :: f (a -> b) -> f a -> f b

This is another way of composing analogous to function application.

Applicatives are more rigid and structured compared to Monads. Monads are much
more flexible as there is no enforcement on the structure. Applicatives enforce
a structure on the computation determined by the structure of the function
application. However, applicatives are more composable than Monads.
Applicatives can be freely composed to create new applicatives whereas monads
cannot be.

Examples
~~~~~~~~

List: apply a collection of functions on a collection of values and
combine the results. Its own unique way of application - apply each
function to each value and then concatenate the results::

  >> [id,id,id] <*> [1,2,3]
  [1,2,3,1,2,3,1,2,3]

IO: Apply the function to the values resulting from the IO action. Note
the function itself is NOT an IO action or something resulting from an
IO action::

  sz <- (++) <$> getLine <*> getLine

Maybe:

Alternative
~~~~~~~~~~~

A monoid on applicative functors. A monoid means we have a way to represent a
zero or identity which means we can perform an action zero or more times and
fold the results into a list combining them in a typeclass instance specific
manner.

The basic intuition is that empty represents some sort of "failure", and (<|>)
represents a choice between alternatives.

Combines applicative actions in the following ways:

+---------------------------+-------------------------------------------------+
| empty                     | Identity of the monoid                          |
+---------------------------+-------------------------------------------------+
| <\|> :: f a -> f a -> f a | In a sequence of actions composed using '<|>',  |
|                           | keep performing actions until you get a         |
|                           | non-empty value.                                |
+---------------------------+-------------------------------------------------+
| some :: f a -> f [a]      | perform an action multiple times, return a list |
|                           | of non-empty results or an empty value.         |
|                           | failure, ...              = failure             |
|                           | success, failure          = success [res]       |
|                           | success, success, failure = sucess [res1, res2] |
+---------------------------+-------------------------------------------------+
| many :: f a -> f [a]      | perform an action multiple times, return an     |
|                           | empty list, a list of values or an empty value. |
|                           | failure, ...              = []                  |
|                           | success, failure          = success [res]       |
|                           | success, success, failure = sucess [res1, res2] |
+---------------------------+-------------------------------------------------+

The intuition is that both some and many keep running `v`, collecting its
results into a list, until it fails; `some v` requires `v` to succeed at least
once, whereas `many v` does not require it to succeed at all. That is, many
represents 0 or more repetitions of `v`, whereas some represents 1 or more
repetitions.

Example: Maybe

+------+----------------------------------------------------------------------+
| <\|> | Perform an action until you get a Just value                         |
+------+----------------------------------------------------------------------+
| some | keep performing until you get a Nothing                              |
+------+----------------------------------------------------------------------+
| many | keep performing until you get a Nothing                              |
+------+----------------------------------------------------------------------+

+--------------+--------------------------------------------------------------+
| some Nothing | Nothing                                                      |
+--------------+--------------------------------------------------------------+
| many Nothing | Nothing                                                      |
+--------------+--------------------------------------------------------------+
| some Just 5  | loop -- because it keeps succeeding every time               |
+--------------+--------------------------------------------------------------+
| many Just 5  | loop -- because it keeps succeeding every time               |
+--------------+--------------------------------------------------------------+

The problem is that since `Just a` is always "successful", the recursion will
never terminate. In theory the result "should be" the infinite list [a,a,a,...]
but it cannot even start producing any elements of this list, because there is
no way for the (<*>) operator to yield any output until it knows that the
result of the call to many will be Just.

In the end, some and many really only make sense when used with some sort of
"stateful" Applicative instance, for which an action v, when run multiple
times, can succeed some finite number of times and then fail. For example,
parsers have this behavior, and indeed, parsers were the original motivating
example for the some and many methods;

Concurrently from the async package has an Alternative instance, for which c1
<|> c2 races c1 and c2 in parallel, and returns the result of whichever
finishes first. empty corresponds to the action that runs forever without
returning a value.

Practically any parser type (e.g. from parsec, megaparsec, trifecta, ...) has
an Alternative instance, where empty is an unconditional parse failure, and
(<|>) is left-biased choice. That is, p1 <|> p2 first tries parsing with p1,
and if p1 fails then it tries p2 instead.

some and many work particularly well with parser types having an Applicative
instance: if p is a parser, then some p parses one or more consecutive
occurrences of p (i.e. it will parse as many occurrences of p as possible and
then stop), and many p parses zero or more occurrences.

* http://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to

Monad
-----

A Monad knows how to flatten the same type contained within the same
type. join eliminates a layer of indirection, the elimination is encoded in a
type specific manner::

  join   :: M (M a) -> M a

It allows functions of type (a -> m b) to be mapped to the type and results
collected by joining. Join behavior defines the Monad::

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  m >>= g = join (fmap g m)

Examples
~~~~~~~~

List: join is concatenation of the resulting list of lists::

  xs >>= f = concat (map f xs) -- concat == join

IO: join is strict evaluation of the IO action (case is strict)::

  bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s
  join x   = x >>= id

do desugar
~~~~~~~~~~

The do notation allows a special form of binding via the ``<-`` symbol. ``<-``
is like a ``=`` in a pattern matching equation except that the binding produced
by ``<-`` must be used in a future computation or action in the same do block
via a bind operator.

+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  do pat <- computation             |  let f pat = more                      |
|     more                           |      f _ = fail "..."                  |
|                                    |  in  computation >>= f                 |
+------------------------------------+----------------------------------------+

+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  do x1 <- action1                  |  action1 >>= (\ x1 ->                  |
|     x2 <- action2                  |    action2 >>= (\ x2 ->                |
|        action3 x1 x2               |        action3 x1 x2))                 |
+------------------------------------+----------------------------------------+

As a special case::

  do
    x1
    x2
    x3
    ...
  is x1 >> x2 >> x3 ...

You can use ``<-`` just like a ``=`` on any expression. For example::

  v <- case x of
        ...

  v <- do
        x1
        x2
        ...

* Each non-let statement in a do statement is bound by the monadic semantics

  * for example in IO monad they are evaluated sequentially
* Each variable bound by "<-" must be chained to another monadic action
* bindings produced by ``<-`` can be used in subsequent let statements in the
  same do block but cannot be used in the where block.

Evaluation semantics
^^^^^^^^^^^^^^^^^^^^

Note that when the monad is strict, each line in the do statement is evaluated
before the next line. However, any let statement evaluation is driven by the
monadic statements where they are used?

For example in the IO monad, action1 is strictly evaluated before action2
irrespective of where x1 or x2 are used in the following code::

  do x1 <- action1
     x2 <- action2
        action3 x1 x2

This is much more clearer from the desugared form of the do statement. Every
`>>=` in the desugared version is an evaluation fence. We go left to right and
anything before a fence is evaluated before anything that comes after it.

Scoping rules
^^^^^^^^^^^^^

Scoping rules for monadic variables. They are not visible in where statements,
but they are visible in the following let statements.

Applicative do
~~~~~~~~~~~~~~

TBD

List Monad Desugaring
~~~~~~~~~~~~~~~~~~~~~

In a list Monad bind is equivalent to `foreach`.

Single argument function::

  do
    x <- [1,2,3]
    return x

  [1,2,3] >>= return

Multi argument function::

  do
    x <- [1,2,3]
    y <- [3,4,5]
    return (x, y)

  [1,2,3] >>= (\x1 -> [3,4,5] >>= (\x2 -> return (x1, x2)))

  The first bind will result in a list:
    [3,4,5] >>= (\x2 -> return (1, x2)
    [3,4,5] >>= (\x2 -> return (2, x2)
    [3,4,5] >>= (\x2 -> return (3, x2)

  Notice that in a multiargument function the first argument is bound first.
  The order may be important depending on the semantics of the monad, for
  example in the IO monad (or any effects monad) the order is important.

  The first bind results in 'foreach x', the second one in 'foreach y'
    foreach x
      foreach y
        return list element (x, y)

Notice how the list monad looks quite like a mini DSL by hiding the details
under the hood.

Monad Intuition
---------------

Monad as Interpreter
~~~~~~~~~~~~~~~~~~~~

A Monad is actually an interpreter.  Each Monad interprets the actions being
performed in it in its own way, and therefore creates a DSL. For example the IO
Monad sequences them, the Maybe monad composes them with error handling
semantics, a list monad combines all elements of a list etc.

The semantics of the interpreter are built into the bind operation.

Bind is a special type of composition which allows you to intercept the
composition and do something extra before passing the value to the next
function.

A regular function application is defined by the runtime system. In a monad the
application is user defined. However the application is enforced to be one
application at a time. In a function application the order of apply is not
defined, they can happen in parallel. However in a monad the order of each
application is fully defined. That is the difference between an identity monad
and pure function application. Identity monad specifies apply order though that
should not be confused with evaluation, it does not guarantee evaluation unless
explicitly enforced by bind implementation.

Even when a function has multiple arguments which one is applied first is
specified by creating lambdas and the do notation helps in doing that
conveniently. Refer to how we created lambdas to curry the arguments out of
order.

The free monad uses a data structure which specifies the application order
which allows us to implement the bind operation later. That's why a free monad
looks like a list, it specifies a sequence.

Monad as Indirection
~~~~~~~~~~~~~~~~~~~~

"We can solve any problem by introducing an extra level of indirection." -
David J. Wheeler.

* Functor is needed to create the type indirection. Functor just helps you
  create correspondences between any types and the indirection. So that you can
  operate on any type in a special mapped world of the functor.
* pure/return just lets you put a value from any regular type into the
  indirection layer so that we can operate on it in the modified composition
  environment. Since applicative and monad are general composition techniques
  we need to put values inside a functor to use the composition. pure/return
  guarantee that we have a way to do that. It lifts a pure value into the type.
* An applicative creates a direct correspondence of a pure function application
  and composition inside the functor.

::

          Functor
            |
            v
       pure/return = guarantee ability to inject values into the functor
            |
            v
        Applicative
            |
            v
          Monad

fmap + return = a -> f b (pure function)

* although Applicative and Functor can be expressed in terms of Monad, they
  still have to be written explicitly.
* liftM is to monad like fmap is to functor

A type introduces a layer of indirection. An indirection allows an abstraction.
A functor type is a very useful indirection as it allows a direct mapping from
any type.

We can perform operations at a layer of indirection. For example, pure or
return can do something when it puts a pure element into the indirection or the
box.  Similarly a monad can do something when it combines two indirections.

With monads as well we have introduced an indirection and do things under the
hood inside the indirection.  Another way of thinking about this is that we
have created boxes around the values, we deal with values and not the boxes.
What is done when these boxes are joined is what defines a monad. For example,
in an IO monad the boundaries of the boxes introduce strict evaluation.

Monad combines functions and does something special when the functions are
composed. Therefore we can use the indirection to pass an invisible state
across all the functions when they are composed. Here the function of the
indirection is handover of the state from one guy to another.

The two tracks
~~~~~~~~~~~~~~

We can also think of monads as compartmentalising our functions inside functor
boxes. Now we have two independent layers. One layer that composes those boxes
together is the monad bind layer. One our regular computation within those
boxes. It is like two tracks running in parallel one is the main track and the
other is auxiliary track behind the scenes. It is like the checked luggage
which arrives when you reach the destination, you do not know how its
transported you just receive it. The luggage could be the shared state.

Another way to think about it is multiple return values. A side effect
producing function actually has more than one return values, the regular pure
return value and a side effect. And we need to compose both. One track composes
the pure value and the other track handles the side effects. Side effects can
be sequenced via composition. In IO monad sequencing is one track and passing
the IO values is another track.

Standard Monads
---------------

* A monad is strict if its >>= operation is strict in its first argument. That
  means it evaluates the result of the previous action before passing it on to
  the next action.

+-----------------------------------------------------------------------------+
| Basic monads defined in the `base` package                                  |
+----------+---------+--------------------------------------------------------+
| Name     | Strict? | Monadic semantics                                      |
+==========+=========+========================================================+
| Identity |         | No additional semantics, just like pure functions      |
|          |         | bind is just a function application.                   |
+----------+---------+--------------------------------------------------------+
| Function |         | Supplies the original value along with the result to   |
|          |         | the next function.                                     |
+----------+---------+--------------------------------------------------------+
| Maybe    | Strict  | Passes on the Just value, stops when it sees Nothing.  |
+----------+---------+--------------------------------------------------------+
| Either   | Strict  | Passes on the right value, stops when it sees Left     |
+----------+---------+--------------------------------------------------------+
| []       | Strict  | Applies every action to all elements of the list       |
+----------+---------+--------------------------------------------------------+
| IO       | Strict  | Evaluate previous action before performing the next.   |
+----------+---------+--------------------------------------------------------+
| ST       | Strict  | Evaluate previous action before performing the next.   |
|          +---------+--------------------------------------------------------+
|          | Lazy    |                                                        |
+----------+---------+--------------------------------------------------------+

Effectful Monads (IO & ST)
--------------------------

A pure function has an explicit and only one output. An effectful function has
a pure output and an effect. The output can be collected, folded, processed
further etc.  Effects are just effects you do not collect them or operate on
them. But there is an operation that is important for effects and that is
"sequence". You can control in what sequence will those effects happen.

IO and ST monads are special in one aspect, they can represent side effects. An
effect can be an IO action or mutating the state of environment in such a way
that can implicitly affect any future operations.

The first requirement for effects is that the monad must be strict i.e. we
evaluate every action completely before we evaluate the next. The strict
evaluation makes sure that any future operations can take the previous effect
into account, or in other words can see the effect. A monad helps us do that by
implementing strictness as the underlying semantics of the monad.

However, we can have pure effect operations which do not generate any explicit
output like a pure function (e.g. a print IO statement). The bind operation of
a monad requires an explicit result from the previous operation to compose the
actions together and implement its semantics.  Pure effects are represented by
a monad by faking an output under the hood even when there is none by using a
`realworld` token.

Open World Effects: The IO Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Strictness ensures that effects `can be` sequenced. However, in case of IO we
also need to enforce that _all_ possible effects are explicitly sequenced with
respect to each other. This is required because the whole world impacted by IO
is considered one global state or one global environment. Therefore all changes
to that global environment must be sequenced.

This is achieved by having the IO monad as a one way type that is you
cannot take values out of it and use them in pure code.  Therefore all IO
actions are guaranteed to be chained or composed together. If we allowed taking
values out of the IO monad then we can go perform some IO effect from pure code
without knowing about it. It creates two problems, (1) there won't be a
guarantee that pure code is really pure, and (2) effects can be performed out
of order with respect to any other effectful operations producing unpredictable
results. Note that this behavior of IO has nothing to do with a monad type. The
one way street is implemented by not exposing the IO constructor and therefore
not allowing a pattern match on it.

Closed World Effects: The ST Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The IO monad assumes an open world i.e.  the state that it operates on is
global for all IO operations. However, the ST monad is designed for closed
world effects i.e. effects are limited to a known isolated state, for example a
mutable array.  Using ST, a pure operation can internally be implemented using
many effectful operations on a closed state.  The ST monad isolates effectful
operations inside it but has a pure interface to the rest of the world.  The
type system (existential quantification) ensures that the internal mutable
state cannot leak out.

+----------+---------+--------------------------------------------------------+
| IO       | Strict  | Evaluate previous action before performing the next.   |
+----------+---------+--------------------------------------------------------+
| ST       | Strict/ | * Embed an opaque mutable data                         |
|          | Lazy    | * Do not allow extraction of the data                  |
+----------+---------+--------------------------------------------------------+

IO Actions - Applicative vs Monad
---------------------------------

IO is an Applicative as well as a Monad, you can use whichever you need.
Applicative and Monad are both ways to compose the sequencing of IO actions.

An applicative is more rigid as the sequencing of actions is tied with function
application. The applicative instance defines the semantics of the side effects
generated by an applicative.

A monad is more flexible, it provides full control of sequencing in the hands
of the programmer. Sequencing of side effects and function applications are
tied together, they can be performed independently, providing more power and
flexibility.

You can think of IO Monad as specifying data dependencies just like an
imperative program has implicit data dependencies. A Monad specifies the
dependencies explicitly.

You can express effectful sequencing using Applicative whereas you can express
effectful looping only using Monads.

Free Functor
------------

::

  newtype Free c a = Free { runFree :: forall b. c b => (a -> b) -> b }

Free Monad
----------

However the free monad detaches the semantics from the bind operation and makes
it a more abstract operation. The semantics are added separately by walking
through the composed structure and interpreting it.

A Monad mixes the structure and the custom DSL interpreter together. A free
monad is more modular, it provides only the structure, the interpreter is added
as a separate layer.

A Free monad is a data type which is constructed using a Functor. It has all
the properties of Applicative and Monad without actually defining any explicit
natural transformations like <*> or >>=. These transfomrations can be
generically defined for the Free data structure which includes a functor.

A free monad does not have a handling customized for a specific type but it is
a monad. That is, it is a bare minimum monad without any custom semantics::

  data Free f a = Pure a | Impure f (Free f a)

f is a functor. This is a recursive data structure which keeps adding one layer
of functor every time. In our earlier definition of a monad we keep eliminating
the extra layer using `join`. Here we keep that layer built into the data
structure and eliminate them at one go later when we consume this data
structure.

It is worth noting that free is a recursive sum type dual to cofree which is
a corecursive product type.  Notice how this structure is like a linked list,
adding nested layers of functors which are to be joined later using a custom
semantics::

  f Free --> f Free --> ... --> Pure.

We have put the constraints on the structure directly rather than using natural
transformations.

+-------------------------------------+---------------------------------------+
| Monad                               | Free Monad                            |
+=====================================+=======================================+
| The operations are written so as to | Computations are automatically        |
| conform to a monadic structure      | structured by embedding them in       |
|                                     | an explicit conforming data structure |
+-------------------------------------+---------------------------------------+
| We have to follow the laws          | The structure ensures the laws        |
+-------------------------------------+---------------------------------------+
| Monad semantics are built along     | Semantics are added as a separate     |
| with the operations                 | layer (the interpreter)               |
+-------------------------------------+---------------------------------------+
| More efficient                      | Indirection always comes at a cost.   |
|                                     | Free monads do not come for  free!    |
|                                     | Though the cost may not always be     |
|                                     | significant.                          |
+-------------------------------------+---------------------------------------+

Freer Monad
-----------

::

     data FFree g a where
       FPure   :: a -> FFree g a
       FImpure :: g x -> (x -> FFree g a) -> FFree g a

Notice the structure of FImpure, a function application coupled with a function
generating `FFree g a`.

Monad vs Comonad
----------------

::

  (=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> (w a -> c)

* Monad composes actions that are producers of functors (`m a` is in output
  position), comonad composes actions that are consumers of functors (`w a` is
  in input position).
* Monadic action produces positive side effects i.e. side effects are in the
  positive position. Comonadic action consumes negative side effects i.e. side
  effects are in negative position.
* Monadic action produces a container or functor layer which is then eliminated
  by a ``join``. Comonadic action consumes a container or functor layer which is
  created by ``duplicate``.
* Monad is provided an environment to run under. Comonad builds an environment?
  that is consumer of environment vs builder of state.
* A Monadic context keeps distributing state to consumers, a comonadic context
  keeps collecting produced state. On the other hand a monadic conetxt collects
  produced effects and a comonadic context produces effects to be collected by
  the actions being composed.
* In a monad the interpreter operates on the state or builds the state, the
  function can take it as input and produce some independent output. In a
  comonad the function builds the state, the interpreter passes it on to the
  function and then takes it out.
* In a monad the function does not know the structure of the state, the
  interpreter knows it and operates on it. It can provide a part of it to the
  function and then take the output of the function and merge it into the
  state.
* In a comonad we have a reverse interpreter. The functions that we are
  composing have a knowledge about the structure of the state and operate on
  it. The interpreter extracts the built copy and then provides an input and
  the accumulated state to the next builder function.
* In a Monad functions produce something and the interpreter assimilates it
  into a larger structure. In a comonad the interpreter produces something and
  the functions assimilate it into a larger structure.
* In a Monad the larger structure is opaque to the functions. Whereas in a
  comonad the larger structure is opaque to the interpreter. That's why it can
  be considered parallel to object oriented programming. The functions embed
  the knowledge of the structure.
* In a Monad the interpreter threads around state carrying functions on the
  side track. In a comonad a state carrying functions is threaded through
  the composing functions and it returns a final value. We then extend that
  returned value to convert it to the function again so that we can feed it to
  the next builder.
* A monad spits out the side effects and the context or the container collects
  and assimilates them in a data structure that it knows about. On the other
  hand in a comonad the actions suck in the side effects from the context  and
  assimilates them in a data structure that it knows about.

* In a monad the two tracks are joined at consumer end of the function i.e.
  both the inputs are provided. In a comonad we extract the other track at
  producing end of the function.
* The m or w in a monad or comonad represents a spiced up state i.e. a value
  with both the tracks, explicit and hidden. A monadic function returns an "m
  a" which means it returns two tracks. Similarly in a comonad we pass "w a"
  which means we are passing two tracks. `m a` or `w a` is a `function` carrying
  state plus explicit value i.e. an overloaded value.

* Comonad has a corecursive structure and monad has a recursive structure.
  monad is like a finite list and comonad like an infinite stream. isn't that
  why hierarchy libraries streaming implementation uses a comonad?

* finite recursive data structures are more likely to fit in a monadic
  structure whereas infinite corecursive data structures fit better in a
  comonadic structure. cellular automata, zippers are some examples of infinite
  comanadic data structures.

* In fact duplicate can be defined naturally as a corecursive data structure
  e.g. this from Dan Piponi's blog:
  >    cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)

* A monad is a linked list of functors (note finite) and a comonad is a stream
  of functors (note infinite).

References
~~~~~~~~~~

* https://wiki.haskell.org/Typeclassopedia
* https://en.wikipedia.org/wiki/Monoidal_category
* https://monadmadness.wordpress.com/2015/01/02/monoids-functors-applicatives-and-monads-10-main-ideas/
* https://arxiv.org/pdf/1406.4823.pdf Notions of Computation as Monoids
* http://stackoverflow.com/questions/35013293/what-is-applicative-functor-definition-from-the-category-theory-pov
* http://stackoverflow.com/questions/17376038/what-exactly-are-the-categories-that-are-being-mapped-by-applicative-functors

* https://wiki.haskell.org/All_About_Monads
* https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/
* http://okmij.org/ftp/Computation/free-monad.html
* https://jaspervdj.be/posts/2012-09-07-applicative-bidirectional-serialization-combinators.html

* http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
* http://gelisam.blogspot.in/2013/07/comonads-are-neighbourhoods-not-objects.html

* https://bartoszmilewski.com/2016/06/15/freeforgetful-adjunctions/
* https://www.schoolofhaskell.com/user/dolio/many-roads-to-free-monads

* http://www.slideshare.net/davidoverton/comonad
* https://bartoszmilewski.com/2017/01/02/comonads/

* https://en.wikipedia.org/wiki/Fundamental_theorem_of_software_engineering
