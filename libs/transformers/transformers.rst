Transformers
============

.. contents:: Table of Contents
   :depth: 1

State Sharing Monads
--------------------

+-------------------------------------------------------------------------------------------------+
| State sharing monads (defined in the `transformers` package)                                    |
+-------------------------------------------------------------------------------------------------+
| The core semantics of the bind operation is to pass a shared state from one                     |
| operation to the next.                                                                          |
+--------+-------------+---------------+-----------------------------+----------------------------+
| Monad  | Transformer | mtl typeclass | Description                 | Typeclass operations       |
|        | Monad       |               |                             |                            |
+========+=============+===============+=============================+============================+
| Reader | ReaderT     | MonadReader   | Read shared state           | ask, reader, local         |
+--------+-------------+---------------+-----------------------------+----------------------------+
| Writer | WriterT     | MonadWriter   | Write to shared state       | tell, writer, pass, listen |
+--------+-------------+---------------+-----------------------------+----------------------------+
| State  | StateT      | MonadState    | Read and write shared state | get, put, state            |
+--------+-------------+---------------+-----------------------------+----------------------------+
| RWS    | RWST        | MonadRWS      | All of the above            | All of the above           |
+--------+-------------+---------------+-----------------------------+----------------------------+

+---------------------------------------+
| Operations on a monadic value         |
+---------------------------------------+
| For example `runReader`, `evalState`  |
+========+=====+=====+===========+======+
| reader | run | map | with      |      |
+--------+     |     +-----------+------+
| write  |     |     | exec      |      |
+--------+     |     +-----------+------+
| state  |     |     | with/exec | eval |
+--------+-----+-----+-----------+------+

State, Reader, Writer monads are defined in terms of Identity monad and the
corresponding monad transformer.

Other Monad Transformers
------------------------

* EitherT
* ExceptT
* ContT
* IdentityT
* MaybeT
* ListT

Stateless vs Stateful
---------------------

A stateless function has no memory, implicit state, context or environment. Its
result purely depends on its arguments. In other words, if it is called with
the same arguments in any context, at any time it yields exactly the same
results every time. A stateless function is also called a `pure` function or
a pure computation.

However, real practical programs are inherently stateful. A ubiquitous example
of state is IO interaction. IO devices represent a global state that the
program is embedded in and has to interact with. Within a program, an example
of state is sharing configuration across various pieces of the program or
passing information back and forth across various components via some shared
state. Another common example of state is error handling, whether we continue
processing may depend on the result of the previous computation.

Stateless computations are easy to reason about, since we do not need to be
aware of the context and the interaction of the computation with the context.
Ideally we would like to keep everything stateless as long as possible but
practically we have to manage state.  However, what we can do is to separate
state management cleanly and modularly from stateless computations. That is
where Haskell is invaluable and different from other languages.

Pure: Stateless Computations
----------------------------

In Haskell, all functions are inherently pure or completely agnostic of state
of any kind.  Pure computations are expressed using functions and expressions
composed of pure functions and evaluated using lazy evaluation. Here is a model
of a statless program::

        Lazy evaluation
        ---------------------------------------------
              pure functions or expressions         Pure Computations
        ---------------------------------------------
          a           b           c           d     Inputs/Outputs

Here, we have assumed that we have a way to make the inputs available to our
program and a way of observing the outputs. We can write completely pure
expressions and functions in GHCi, but to have a meaningful standalone program
we will need a stateful interaction with the environment (global state)
outside the program using IO.

::

  >> let add x y = x + y
  >> add 1 2
  3

Monads: Stateful Computations
-----------------------------

TBD - maybe example

A monad `m` is an abstraction or context that can wrap values of some type `a`
as `m a` and implements a stateful logic to combine the wrapped values.  The
state management or combining logic is hidden under the hood in the
implementation of the monad `m`. The programmer writes pure stateless functions
of type `f :: a -> m b` that map a pure value `a` to a monadic or state aware
value `m b`. These functions are then combined using a bind (`>>=`) operation
like `s >>= f1 >>= f2 >>= f3` where `s :: m a` is the initial state and the
value of this expression is the final state `t :: m b`.

`>>= :: m a -> (a -> mb) -> m b`

The programmer only needs to know the semantics of the monad's stateful
processing. The state management is handled under the hood and therefore is
separated from pure processing in a modular and transparent way. The programmer
writes stateful code by combining stateless functions. The monadic functions or
actions can be thought of as state transition maps oblivious of the actual
state. The state flows through the chain of actions, being inspected or
modified by each one of them and finally producing the resulting state. The
bind operation makes sure that all the actions in a monad are totally ordered
and chained together.

::

        ---------------------------------------------
              pure functions or expressions          Stateless Pure Layer
        ---------------------------------------------
          a           b           c           d      Pure Inputs/Outputs
          ^           ^           ^           ^
        -||----------||----------||----------||------
         m a         m b         m c         m d
        ---------------------------------------------
        Monad evaluation semantics
        vertically compartmentalized by bind
        ---------------------------------------------Stateful Monad Layer
  Actions vs    \->   vf1    \->   vf2    \->   f3v
        ---------------------------------------------

Interface of a Monad
--------------------

+-----------------------------------------------------------------------------+
| Types of primitive operations provided by a monad to implement the actions. |
+--------------------------+---------+----------------------------------------+
| Primitive class          | Generic | Example                                |
+==========================+=========+========================================+
| create state (wrap `a`)  | Yes     | ``return   :: a -> m a``               |
+--------------------------+---------+----------------------------------------+
| eliminate state          |         | ``runState :: m a -> a``               |
| (unwrap `a`)             |         +----------------------------------------+
|                          |         | ``fromMaybe :: a -> m a -> a``         |
+--------------------------+---------+----------------------------------------+
| read state               |         | ``get     :: m a``                     |
|                          |         +----------------------------------------+
|                          |         | ``getLine :: m a``                     |
+--------------------------+---------+----------------------------------------+
| modify state             |         | ``put       :: a -> m b``              |
|                          |         +----------------------------------------+
|                          |         | ``putStrLn  :: a -> m b``              |
+--------------------------+---------+----------------------------------------+

The primitives are used to create composite actions using the ``>>=``
operation.  Each one of the component actions being bound must be
of type `m a`.  A composite action is essentially of this form::

  f :: ... -> m a
  f ... = ... >>= action1 >>= action2 >>= action3 ...

Evaluation and Interpreter
~~~~~~~~~~~~~~~~~~~~~~~~~~

A monad orders all its actions in a sequence. If the bind is strict in its
first argument, the evaluation proceeds step by step, each action is fully
evaluated before the next one. This is quite like an interpreter. The bind
statement is an interpreter which implements the the underlying semantics of
the monad and executes the actions in sequence one by one. That's why monads
are useful in implementing interpreted DSLs.

Examples
~~~~~~~~

Different types of monads have different state semantics that serve specific
purposes.  For example, in a given action sequence, the `IO` monad evaluates
the previous action fully before performing the next action, the `Maybe` monad
performs the next action only if the previous one was successful, a `Reader`
monad passes some configuration or environment values from the previous action
to the next, allowing sharing of a common environment by all actions.

::

  do
    x <- getLine
    let y = doSomething x
    putStrLn y

Maybe Monad
-----------

The action chain results in a Maybe value, a Just if all actions return Just or
a Nothing if any of them returns Nothing. Finally the result can be unwrapped
by using `fromMaybe`::

  instance  Monad Maybe  where
      return              = Just

      (Just x) >>= k      = k x
      Nothing  >>= _      = Nothing

::

  Pure       x      |       y     |                          a
             ^      |       ^     |
           --|------|-------|-----|--------------------------
 Result      Just x |      Just y |       Nothing
           ---------|-------------|--------------------------
 Actions     f1    >>=     f2    >>=         f3   >>=    f4   (Maybe a)
           ---------|-------------|--------------------------
                    eval f1    eval f2
                    WHNF        WHNF

Pure values can be lifted into the monad using `return`.

Example: A list of integers, perform a running sum, abort if it ever becomes
negative.

Lifting a Function into a Monad
-------------------------------

A lifted function must work on monadic arguments and return a monadic result,
which means we need to evaluate the arguments first and then apply them to the
function::

  liftM2  :: (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
  liftM2 f m1 m2 = do
    x1 <- m1
    x2 <- m2
    return (f x1 x2)

          x1          x2
          ^           ^                        Pure Layer
        --|-----------|---------||--
 Actions  m1   >>=    m2   >>=  vv f => m r    Monadic Layer (Magic)
        ----------------------------

General Structure of a stateful program
---------------------------------------

It will be a tree like structure having IO at the root::

          S5     S6                    S8
          ----- ---- ---------  -----------------------
  Internal S3    S4    S5              S7
  States  ----- ----- --------  -----------------------
                 S1                       S2
          --|-----------|-----------|-----------|------
   IO Act   A    >>=    B    >>=    C    >>=    D
          ---------------------------------------------
          External world

The state that IO operates on is global and external to the program.  To be
meaningful the program has to do some form of IO, and we can never extract
values from IO (that's why main always has the type IO). Therefore IO has to be
always at the bottom, and any code that performs IO must be in IO monad all the
way up to main. IO can never be run from pure code.

All other monads operate on local states internal to the program, we can run the
monad under some local state from pure code and even retrieve the final state
when needed.

Combining Stateful Semantics
----------------------------

Let us say we want to run some IO actions but at the same time want to use the
error handling behavior of the Maybe monad to abort when an error or stop
condition, dependent on the value we retreived from IO, is encountered.

What we need is to interleave IO and Maybe such that they both work in tandem,
IO lifting the value to a Maybe and Maybe performing the job of stopping any
further processing as soon as a Nothing is encountered.

We can make our IO actions return maybe values instead of plain values i.e.
``IO (Maybe a)`` instead of ``IO a``. A just value indicates no error and
Nothing indicates there was an error. Then we can run these actions with the
semantics of the Maybe monad.

::

             x        |   y       |                       a
 Pure        ^        |   ^       |
           --|--------|---|-------|-----------------------
 Result      Just x   |   Just y  |   Nothing
           -----------|-----------|-----------------------
 Maybe       W      |     X     |      Y           Z      (Maybe a)
             ^      |     ^     |      ^           ^
           --|------|-----|-----|------|-----------|------
 IO Action   f1     | >>= f2    | >>=  f3   >>=    f4     IO (Maybe a)
           ---------|-----------|-------------------------
                   WHNF         WHNF
                   eval f1      eval f2

We have two layers here. The lower IO layer produces values according
to IO monad semantics, these values are then lifted into a Maybe type. The
Maybe monad layer then composes these according to the Maybe semantics. So we
can use the regular Maybe asbtractions and tools on top of the IO values.

Evaluation is an important aspect of the semantics of a monad. The lowest monad
drives evaluation. If the lowest monad is strict, a bind in that will force
evaluation of that whole vertical compartment. If the lower one is lazy then
the next one will drive the evaluation.

In this particular case the way we think about the evaluation is that the IO
bind occurs first in sequence which forces the bind of Maybe, which forces the
evaluation of the expressions in the vertical compartment.

Transformer Stack
-----------------

We did this as a custom solution, but can we do this for any monad and not
just IO monad?

We use `TransT` as a generic transformer definition just to illustrate the
generic structure of a transformer. In the text below, we represent the
transformed monad `TransT m` by the variable `t` and the lower monad by the
variable `m`.

::

  newtype TransT m a = TransT {runTransT :: m (StT   a) }
  newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a) }

  instance Monad      (TransT m) where ...   -- the transformed monad
  instance MonadTrans  TransT    where ...   -- the transformer

The transformer type `TransT` transforms a monad `m` into a combined monad of
type (`TransT m`) adding new semantics on top of `m`. We call `m` as the lower
level monad and `TransT m` as the top level monad.

The runtime representation of the combined type is `m StT a`, where `StT` is
the transformer specific data wrapper. Since the outermost constructor of this
type is `m` we use a type level wrapper `TransT` to represent the combined type
as a newtype.

The run function `runTransT` runs or unwraps top level transformer monad
`TransT m a`, yielding the value in underlying monad `m (StT a)`::

  runTransT :: TransT m a -> m (StT a)
  runMaybeT :: MaybeT m a -> m (Maybe a)

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer.png

The combined type can be wrapped again inside another transformer monad and so
on, forming a stack of monads. Stacking monads in this way allows us to combine
multiple monads together interleaving the functionality of all of them
together.

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-stack2.png

The Two tracks
~~~~~~~~~~~~~~

The e and a in a transformer represent the two tracks of a monad. The e track
is the hidden track and a is the normal track::

  newtype TransT e m a = TransT {runTransT :: m (StT e a) }

MonadTrans (lift) - Lifting through the Stack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A transformer monad provides a ``lift`` operation which allows lifting values
from an arbitrary monad ``m`` to the transformer monad thus transforming ``m``
into this monad. When we have a stack of transformer monads we can apply
``lift`` in a cascading manner to lift from a lower monad in the stack to a
higher monad.

Running a computation in `m` yields a result of type `m a`. To be able to
use that result in `t m` we need to know how to wrap that into our type wrapper
so as to construct a `t m a` type from that.

The `MonadTrans` class allows us to do the wrapping generically for any
transformer. Every transformer `t` provides an instance of MonadTrans.
MonadTrans provides a `lift` operation which knows how to wrap a value `m a`
from an arbitrary monad `m` into the `t` monad::

  class MonadTrans t where -- t represents TransT here
    lift :: m a -> t m a

  -- lifting an 'm a' into 'MaybeT m a'
  instance MonadTrans MaybeT where
      lift = MaybeT . liftM Just
      -- this is just the lifted Just with a MaybeT wrapper
      -- Compare with Maybe monad's 'return = Just'

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-lift.png

The way `return` lifts pure values into the Maybe monad, the same way `lift`
lifts values from the `m` monad into `MaybeT`. lift generalizes the return
operation of a monad. In fact return for a transformer is defined in terms of
`lift`::

    return = lift . return

A transformer can wrap any monad generically. Also, it is agnostic of the full
stack of transformers, all it needs to know is the immediate next monad that it
is wrapping.

By applying lift in a cascading manner we can wrap a value from a monad lower
down in the stack to the desired level. What is the use case for this?

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-lift2.png

The `transformers` package provides monad transformer types and MonadTrans
instances for all the standard monads (``IO, Maybe, Either, [], (->),
Identity``).

Lifting and Lowering
~~~~~~~~~~~~~~~~~~~~

Transformers provide a monad agnostic way of lifting computations from one
monad to another. The opposite of lifting is lowering which happens when we
invoke the monad runner function. However sometimes we may need to run a lifted
computation in a lower level monad. In that case we may have to invoke the
monad runner in the lower level monad to lower the computation. monad-control
and monad-unlift packages provide a generic way of lowering the computations
for this purpose.

MonadTransControl (liftWith)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lifting functions through the transformer stack, when those functions may need
to run computations in the current monad.

`MonadTransControl` provided by the `monad-control` package is a more flexible
and powerful version of MonadTrans.

`liftWith` is a more powerful `lift`. lift allowed us to run an action in the
wrapped monad `m` and then bring in the result value from `m` to the
transformer monad `t`.  `liftWith` provides a `Run` function that allows
running `t` computations embedded inside the `m` computations being lifted.
This enables us to capture bindings of `t` computations inside the `m`
computations and run them using `Run`.  `restoreT` allows constructing a `t`
computation from the result of a `Run t` function, therefore bringing the
results of `t` computations from `m` back into `t`. This allows interleaving of
`m` and `t` computations freely and generically.

::

   ------------------------
  |  t (MonadTransControl) |  ^
   ------------------------   | liftWith :: (Run t -> m a) -> t m a
   ------------------------   | restoreT :: m (StT t a)    -> t m a
  |  m                     |
   ------------------------

`MonadTransControl` class essentially lets us specify the structure of a
transformer generically to be able to wrap (construct) and unwrap (run) the
type using generic functions. The wrapped type is specified using the
associated type `StT t a`, the run (unwrap) function type is derived from this.
The constructor for the type is specified using `restoreT`.  Let's take the
example of ``MaybeT`` instance and see how this works::

  newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

  instance MonadTransControl MaybeT where
   -- the type that is wrapped inside m (i.e. Maybe a)
   -- type StT t      a  :: *
      type StT MaybeT a  =  Maybe a

   -- Using this associated type we can construct
   -- the type of the run function for MaybeT (i.e. runMaybeT)
   -- type Run t      = t      n b -> n (StT t b)
   -- type Run MaybeT = MaybeT n b -> n (Maybe b)

   -- the function 'f' composes an action in the 'm' monad.
   -- liftWith executes that action and lifts the result back into 'MaybeT'.
   -- 'f' is passed the run function of MaybeT (i.e. runMaybeT) that
   -- allows us to run 'MaybeT n' computations inside 'f'.

   -- liftWith   :: (Run t -> m a) -> t m a
      liftWith f = MaybeT (liftM return (f runMaybeT))

   -- For example:
   -- f :: Run t -> m a
   -- f run = return ()
   -- f run = return . g . run
   -- f run = run t

   -- We can also extract the run function and apply it later
   -- f r = return r
   -- run <- liftWith f

   -- Constructing a MaybeT. This can be used to reconstruct a
   -- MaybeT from a value returned by 'liftWith'
   -- restoreT :: m (StT t a) -> t m a
   -- restoreT :: m (Maybe a) -> t m a
      restoreT  a = MaybeT a

Instances for standard monads are provided by the monad-control package.

MonadIO (liftIO) - Lifting from IO to some Monad
------------------------------------------------

Note: This mechanism is completely independent of transformer Monads. Helps in
writing generic monad code. As long as the monad running the code provides a
MonadIO instance we can use liftIO to lift values from IO to the monad. However
this can be combined with ``lift`` to lift an IO action through a transpformer
stack, providing a MonadIO instance for any transformer.

The `MonadIO` class provides an abstraction `liftIO` to lift a value from the
IO monad to monad `m`::

  class (Monad m) => MonadIO m where
      liftIO :: IO a -> m a

Using the `lift` abstraction a transformer can implement `liftIO` by lifting
the value iteratively through the whole stack until we reach the IO Monad::

  instance (MonadIO m) => MonadIO (MaybeT m) where
   -- liftIO :: IO a -> MaybeT m a
      liftIO =   lift    -- lift from m to (MaybeT m)
               . liftIO  -- liftIO from IO to m

When we reach the IO Monad the iteration stops because `liftIO` for the IO
monad is just `id`::

  instance MonadIO IO where
      liftIO = id

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-io-lift2.png

We can write functions which are polymorphic in the monad type and therefore
work for any monad. We can use class constraints to make sure that the monad
and the whole stack under it support lifting from IO.  For example::

  f :: (MonadIO m) => ... -> m a
  res <- liftIO getLine
  ...

MonadBase (liftBase) - Lifting from a designated Base Monad
-----------------------------------------------------------

Note: This mechanism is completely independent of transformer Monads. Helps in
writing generic monad code. However this can be combined with ``lift`` to lift
an IO action through a transformer stack, providing MonadBase instance for any
transformer.

`MonadBase` generalizes `MonadIO` to any monad.  The `MoandBase`
class provides a `liftBase` operation to lift values from an arbitrary base
monad `b` to the current monad `m` as long as we have a `MonadBase b m`
instance::

  class MonadBase b m where
    liftBase :: b a -> m a

Using the `lift` abstraction a transformer can implement `liftBase` generically
by lifting the value iteratively through the whole stack until we reach the
base monad::

  instance (MonadBase b m) ⇒  MonadBase b (TransT m) where
    liftBase =   lift     -- lift from m to (TransT m)
               . liftBase -- lift from b to m

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-base-lift2.png

When we reach the base Monad the iteration stops because `liftBase` for the
base monad is just `id`::

  instance MonadBase b b where liftBase = id

For a polymorphic function we can use a `MonadBase b b` constraint to sepcify
the base monad relationship.  The `transformers-base` package provides
`MonadBase b b` and `MonadBase b m` instances for all combinations of `b` and
`m` for the standard monads.  For user defined transformers the MonadBase
instance can be derived automatically::

  deriving instance (MonadBase b m) => MonadBase b (TransT m)

For example::

  f :: (MonadBase m) => ...
  res <- liftBase baseOperation

MonadBaseControl (liftBaseWith)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lifting functions from a designated Base Monad, functions being lifted may need
to run computations in the current monad.

`MonadBaseControl` is a more flexible and powerful version of `MonadBase`.

While lifting computations in `b`, `liftBaseWith` provides a `RunInBase`
function, which is a runner function for the monad `m` allowing us to run `m`
computations embedded inside `b` computations; `restoreM` allows constructing
an `m` value back from the results returned by `RunInBase`.  This allows us to
capture bindings from surrounding `m` computations inside `b` computations and
run them while lifting `b`::

   ------------------------
  |  m (MonadBaseControl)  |  ^
   ------------------------   |
                              |
                              |  liftBaseWith :: (RunInBase m b -> b a) -> m a
   ------------------------   _  restoreM :: StM m a -> m a
  |  b (MonadBaseControl)  |
   ------------------------

  type RunInBase m b = forall a. m a -> b (StM m a)

This mechanism allows us to lift arguments of functions and not just the
results, for example we can lift `catch` using this. Notice that the arguments
too are actions and have a polymorphic `m a` type. `control` is a convenience
function which calls `restoreM` after `liftBaseWith`::

  catch :: (MonadBaseControl IO m, Exception e)
        => m a        -- ^ The computation to run
        -> (e -> m a) -- ^ Handler to invoke if an exception is raised
        -> m a
  catch a handler = control $ \runInIO ->
                      E.catch (runInIO a)
                              (\e -> runInIO $ handler e)

Instances for standard monads are provided by the monad-control package.

MonadTransUnlift
~~~~~~~~~~~~~~~~

For a readonly sharing transformer, simpler versions of running an action in
the lower monad. Note, readonly transformers can have mutable IORefs to
keep the state readonly but still provide RW capabilities.

askRun - get the run function
askUnlift - get `Unlift run`

MonadBaseUnlift
~~~~~~~~~~~~~~~

Run an action in a base monad:

askUnliftBase - get `UnliftBase run`

Summary
~~~~~~~

+--------------------------------------------------------------------------------------------+
| Summary of lifting operations in a transformer stack                                       |
+--------------+-------------------+---------------+-----------------------------------------+
| Package      | Typeclass         | Operations    | Description                             |
+==============+===================+===============+=========================================+
| base         | MonadIO           | liftIO        | lift a computation from the IO monad    |
+--------------+-------------------+---------------+-----------------------------------------+
| transformers | MonadTrans        | lift          | lift from the argument monad to the     |
|              |                   |               | result monad                            |
+--------------+-------------------+---------------+-----------------------------------------+
| transformers-| MonadBase         | liftBase      | lift a computation from the base monad  |
| base         |                   |               |                                         |
+--------------+-------------------+---------------+-----------------------------------------+
| monad-control| MonadTransControl | liftWith,     | lift carrying the state of current monad|
|              |                   | restoreT      | restoreT can restore the state.         |
|              +-------------------+---------------+-----------------------------------------+
|              | MonadBaseControl  | liftBaseWith, | lift base with state                    |
|              |                   | restoreM      |                                         |
+--------------+-------------------+---------------+-----------------------------------------+
| monad-unlift | MonadTransUnlift  | askUnlift,    |                                         |
|              |                   | askRun        |                                         |
|              +-------------------+---------------+-----------------------------------------+
|              | MonadBaseUnlift   | askUnliftBase,|                                         |
|              |                   | askRunBase    |                                         |
+--------------+-------------------+---------------+-----------------------------------------+

What is lifting?
----------------

In general, lifting is wrapping a type into some sort of a `box` around it,
creating a layer of indirection or a semantic context around the type.  Lifting
takes place at many levels and in many forms.  The most basic example is
`lifted types`, where the box is a closure structure on the heap which helps
lazy construction of the type.  In almost all other cases the box is a functor
(keep in mind that applicative and monad are also functors).

The `pure` and `return` statements basically lift a pure value into an
applicative or a monad respectively. We are essentially wrapping a type into a
functor. While a monad wraps pure values, a monad transformer wraps monadic
types instead, we lift values into the transformer type by using the `lift`
operation on a monadic type.

Lifting merely adds more context around an opaque type and never loses any
information from the original value. Put another way, lifting uses only
constructors and no pattern matches.

+-----------------------------------------------------------------------------+
| Summary of value lifting operations                                         |
+---------------+--------+----------+-----------------------------------------+
| Operation     | From   | To       | Description                             |
+===============+========+==========+=========================================+
| pure          | a      | f a      | Lift a type into an applicative functor |
+---------------+--------+----------+-----------------------------------------+
| return        | a      | m a      | Lift a type into a monad                |
+---------------+--------+----------+-----------------------------------------+
| lift          | m a    | t m a    | lift from a lower monad to the upper    |
|               |        |          | transformer monad.                      |
|               |        |          | ``t m`` is a transformer monad          |
+---------------+--------+----------+-----------------------------------------+
| liftIO        | IO a   | m a      | lift a value from the IO monad to m.    |
|               |        |          | m must satify MonadIO m                 |
+---------------+--------+----------+-----------------------------------------+
| liftBase      | b a    | m a      | lift a value from monad b to monad m.   |
|               |        |          | m must satify MonadBase b m             |
+---------------+--------+----------+-----------------------------------------+

Lifting Functions
-----------------

+--------------------------------------------------------------------------------------------+
| Summary of function lifting                                                                |
+---------------+--------------+-------------------+-----------------------------------------+
| Operation     | From         | To                | Description                             |
+===============+==============+===================+=========================================+
| fmap          | (a -> b)     | f a -> f b        | Lift a function into a functor          |
+---------------+--------------+-------------------+-----------------------------------------+
| liftA         | (a -> b)     | f a -> f b        | Lift a function into an applicative     |
+---------------+--------------+-------------------+ functor.                                |
| liftA2        | (a -> b -> c)| f a -> f b -> f c |                                         |
+---------------+--------------+-------------------+-----------------------------------------+
| liftM         | (a -> b)     | m a -> m b        | Lift a function into a monad            |
+---------------+--------------+-------------------+                                         |
| liftM2        | (a -> b -> c)| m a -> m b -> m c |                                         |
+---------------+--------------+-------------------+-----------------------------------------+

For functions, lifting means coverting a function that works on unlifted
arguments into a function that workds on lifted argument types and returns a
lifted type.

Simple rules to use transformers:

* use a transformer just like any regular monad, you do not need to care about
  the underlying monad, regular monads work on pure values, transfomers can
  work on pure values or values lifted from inner monad or from some base
  monad.

How values in a monad are generated?
  * lifting values
  * functions producing values of those types

mtl
---

`mtl` is a convenience add-on on top of the `transformers` package. It extends
transformers so that you do not have to lift operations explicitly.

It provides classes for each monad like `MonadReader`, `MonadWriter`,
`MonadState`.  Each monad is made an instance of all other monad classes
therefore providing functions of all from any of the monad. The functions are
defined as lifted using the lift operations from the transformers library.
Therefore mtl adds the convenience of not having to lift operations yourself.

Any monad which implements the `MonadReader` class can use operations from a
reader buried somewhere down the transformer stack without explicit lifting.
For example we can just use the `ask` operation and it will retrieve the
environment of a reader somewhere down in the stack.

Provide an example class here.

Extensible Exceptions
---------------------

* https://hackage.haskell.org/package/exceptions Extensible optionally-pure
  exceptions

* MonadThrow throwM
* MonadCatch catch
* MonadMask mask

Safe Exceptions
---------------

Packages
--------

* base
* transformers
* transformers-base
* monad-control
* lifted-base
* lifted-async
* monad-unlift

References
-----------

* https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html
* https://www.schoolofhaskell.com/user/jwiegley/monad-control
* http://www.yesodweb.com/book/monad-control
* https://hackage.haskell.org/package/safe-exceptions
* https://github.com/fpco/safe-exceptions/blob/master/COOKBOOK.md
