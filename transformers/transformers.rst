Transformers
============

.. contents:: Table of Contents
   :depth: 1

State Sharing Monads
--------------------

+-------------------------------------------------------------------------------------------------+
| State sharing monads (defined in `transformers` package)                                        |
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

Transformer Stack
-----------------

A transformer monad (`TransT`) embeds its state (`StateT`) inside some `inner`
monad `m`.  The inner monad is wrapped inside the transformer monad using a
newtype wrapper::

  newtype TransT m a = TransT {runTransT :: m (StateT a)} deriving Monad
  newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a) } deriving Monad

The run function `runTransT` runs or unwraps the outer transformer monad,
yielding a value of type `m (StateT a)`::

  runTransT :: TransT m a -> m (StateT a)
  runMaybeT :: MaybeT m a -> m (Maybe a)

We are using the `TransT` definition as a generic definition just for
illustration. In the text below, we represent the outer transformer monad by
the variable `t` and the inner monad by the variable `m`.

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer.png

This combined type can be wrapped again inside another transformer  and so on,
forming a stack of monads. Stacking monads in this way allows us to combine
multiple monads together and use the functionality of all of them together.
The innermost monad in the whole stack, which does not wrap any other monad, is
called the `base monad`.

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-stack2.png

MonadTrans (lift)
~~~~~~~~~~~~~~~~~

When we run a computation in `m` we get a result of type `m a`. To be able to
use the result in `t` we need to know how to wrap that into our type wrapper to
construct a `t m a` type from that.

The `MonadTrans` class allows us to do the wrapping generically for any
transformer. Every transformer `t` provides an instance of MonadTrans.
MonadTrans provides a `lift` operation which knows how to wrap a value `m a`
from an arbitrary monad `m` into the `t` monad.

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-lift.png

`lift` is nothing but applying the wrapper function `TransformerT` to the type
`m a` in a manner appropriate for the given transformer type `t`::

  lift :: m a -> t m a
  -- lifting an 'm a' into 'MaybeT m a'
  instance MonadTrans MaybeT where
      lift = MaybeT . liftM Just

By applying lift multiple times we can wrap a value from a monad lower down in
the stack to the desired level.  The `transformers` package provides monad
transformer types and MonadTrans instances for all the standard monads (IO,
Maybe, Either, [], (->), Identity).

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-lift2.png

MonadBase (liftBase)
~~~~~~~~~~~~~~~~~~~~

The innermost monad in a stack, the one not wrapped by any other monad, is
called the base monad. For the common case of lifting from the base monad, the
`MonadBase b t` instance provides a `liftBase` operation to lift from `b` to
`t`::

  liftBase :: b a -> t a

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-base-lift2.png

For a base monad (instance `MonadBase b b`), `liftBase` is usually just `id`
since we are lifting to the same monad.  For a transformer it is `lift .
liftBase`. The MonadTrans class has already provided us the necessary lift
operation to implement liftBase.

The `transformers-base` package provides MonadBase instances for base as well
as transformer versions of all the standard monads. For user defined
transformers the MonadBase instance can be derived automatically::

  deriving instance (MonadBase b t) => MonadBase b (TransformerT t)

For example::

  f :: (MonadBase m) => ...
  res <- liftBase baseOperation

MonadIO (liftIO)
~~~~~~~~~~~~~~~~

For lifting from IO to any monad we have a special `MonadIO` typeclass that
provides us the `liftIO` operation. Though the same job can be done by the
MonadBase typeclass as well::

    liftIO :: IO a -> t a

.. image:: https://github.com/harendra-kumar/concise-haskell-diagrams/blob/master/transformers/transformer-io-lift2.png

For example::

  f :: (MonadIO m) => ...
  res <- liftIO getLine

MonadTransControl (liftWith)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

MonadBaseControl (liftBaseWith)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`MonadBaseControl` is a more flexible and powerful version of `MonadBase`.

`liftBaseWith` provides a `RunInBase` function to the `b` computation being
lifted. `RunInBase` is a runner function for the `m` monad and allows us to run
`m` computations embedded inside the `b` computations. This allows us to
capture bindings from `m` inside the `b` computations and run them while
lifting `b`. `restoreM` allows constructing a `m` value back from the results
returned by `RunInBase`::

   ------------------------
  |  n (MonadBaseControl)  |    ^
   ------------------------     |
  |  m (MonadBaseControl)  |  ^ |
   ------------------------   | |
                              | |
                              | | liftBaseWith :: (RunInBase m b -> b a) -> m a
   ------------------------   _ _ restoreM :: StM m a -> m a
  |  b (MonadBaseControl)  |
   ------------------------

  type RunInBase m b = forall a. m a -> b (StM m a)

This mechanism allows us to lift arguments of functions and not just the
results, for example we can lift `catch` using this. Notice that the arguments
too are actions and have a generic `m a` type. `control` is a convenience
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

MonadBaseUnlift
~~~~~~~~~~~~~~~

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
