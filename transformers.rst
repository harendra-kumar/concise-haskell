Transformers
============

Transformer Stack
-----------------

A transformer monad is created by wrapping a monad inside a newtype wrapper and
by deriving an instance of Monad.  For example::

  -- newtype TransformerT m a = TransformerT {runTransformerT :: ...         }
     newtype MaybeT       m a = MaybeT       { runMaybeT      :: m (Maybe a) }

The type constructor `TransformerT`, in short represented by `t`, is the
transformer monad and `m` is an arbitrary monad wrapped in `t`. `m a` is the
wrapped type, `TransformerT` is the type wrapper, `runTransformerT` is the
unwrapping function, which yields the type `m a` when run on a `t` value.

This combined type can be wrapped again inside another type  and so on, forming
a stack of monads. This stacking allows us to combine multiple monads together
and use the functionality of all of them together.  The innermost monad in the
whole stack, which does not wrap any other monad, is called the `base monad`.

[TBD] circular rings picture here.

MonadTrans (lift)
~~~~~~~~~~~~~~~~~

When we run a computation in `m` we get a result of type `m a`. To be able to
use the result in `t` we need to know how to wrap that into our type wrapper to
construct a `t m a` type from that.

The `MonadTrans` class allows us to do the wrapping generically for any
transformer. Every transformer `t` provides an instance of MonadTrans.
MonadTrans provides a `lift` operation which knows how to wrap a value `m a`
from an arbitrary monad `m` into the `t` monad::

   -----------------
  |  t (MonadTrans) |  ^
   -----------------   | lift :: m a -> t m a
  |  m              |
   -----------------

`lift` is nothing but applying the wrapper function `TransformerT` to the type
`m a` in a manner appropriate for the given transformer type `t`::

  -- lifting an 'm a' into 'MaybeT m a'
  instance MonadTrans MaybeT where
      lift = MaybeT . liftM Just

By applying lift multiple times we can wrap a value from a monad lower down in
the stack to the desired level.  The `transformers` package provides monad
transformer types and MonadTrans instances for all the standard monads (IO,
Maybe, Either, [], (->), Identity).

MonadBase (liftBase)
~~~~~~~~~~~~~~~~~~~~

The innermost monad in a stack, the one not wrapped by any other monad, is
called the base monad. For the common case of lifting from the base monad, the
`MonadBase b m` typeclass provides a `liftBase` operation to lift from `b` to
`m`::

   --------------------
  |  n (MonadBase b n) |
   --------------------    ^
  |  m (MonadBase b m) |   |
   --------------------  ^ |
                         | | liftBase :: b a -> m a
                         | |
   --------------------  - -
  |  b (MonadBase b b) |
   --------------------

For a base monad (instance `MonadBase b b`), `liftBase` is usually just `id`
since we are lifting to the same monad.  For a transformer it is `lift .
liftBase`. The MonadTrans class has already provided us the necessary lift
operation to implement liftBase.

The `transformers-base` package provides MonadBase instances for base as well
as transformer versions of all the standard monads. For user defined
transformers the MonadBase instance can be derived automatically::

  deriving instance (MonadBase b m) => MonadBase b (TransformerT m)

For example::

  f :: (MonadBase m) => ...
  res <- liftBase baseOperation

MonadIO (liftIO)
~~~~~~~~~~~~~~~~

For lifting from IO to any monad we have a special `MonadIO` typeclass that
provides us the `liftIO` operation::

   --------------
  |  n (MonadIO) |
   --------------    ^
  |  m (MonadIO) |   |
   --------------  ^ |
                   | |
                   | | liftIO :: IO a -> m a
   --------------  - -
  |  IO          |
   --------------

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
results, for example we can lift `catch` using this::

  catch :: (MonadBaseControl IO m, Exception e)
        => m a        -- ^ The computation to run
        -> (e -> m a) -- ^ Handler to invoke if an exception is raised
        -> m a
  catch a handler = control $ \runInIO ->
                      E.catch (runInIO a)
                              (\e -> runInIO $ handler e)

Instances for standard monads are provided by the monad-control package.

Summary
~~~~~~~

+-----------------------------------------------------------------------------+
| Summary of lifting operations in a transformer stack                        |
+--------------------+--------------+-----------------------------------------+
| Typeclass          | Operations   | Description                             |
| (package)          |              |                                         |
+====================+==============+=========================================+
| MonadIO (base)     | liftIO       | lift a computation from the IO monad    |
+--------------------+--------------+-----------------------------------------+
| MonadTrans         | lift         | lift from the argument monad to the     |
| (transformers)     |              | result monad                            |
+--------------------+--------------+-----------------------------------------+
| MonadBase          | liftBase     | lift a computation from the base monad  |
| (transformers-base)|              |                                         |
+--------------------+--------------+-----------------------------------------+
| MonadTransControl  | liftWith,    | lift carrying the state of current monad|
| (monad-control)    | restoreT     | restoreT can restore the state.         |
+--------------------+--------------+-----------------------------------------+
| MonadBaseControl   | liftBaseWith,| lift base with state                    |
| (monad-control)    | restoreM     |                                         |
+--------------------+--------------+-----------------------------------------+

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

State, Reader, Writer are defined in terms of Identity and the monad
transformer.

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

References
-----------

* https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html
* https://www.schoolofhaskell.com/user/jwiegley/monad-control
* http://www.yesodweb.com/book/monad-control
* https://hackage.haskell.org/package/safe-exceptions
* https://github.com/fpco/safe-exceptions/blob/master/COOKBOOK.md
