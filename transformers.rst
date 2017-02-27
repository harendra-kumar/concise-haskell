Standard Monads
---------------

* A monad is said to be strict if its >>= operation is strict in its first
  argument. The base monads Maybe, [] and IO are strict:

+-----------------------------------------------------------------------------+
| Basic monads defined in the `base` package                                  |
+----------+---------+--------------------------------------------------------+
| Name     | Strict? | Semantics                                              |
+----------+---------+--------------------------------------------------------+
| Identity |         |                                                        |
+----------+---------+--------------------------------------------------------+
| []       | Strict  | perform actions for all elements of list               |
+----------+---------+--------------------------------------------------------+
| Maybe    | Strict  | Stop performing actions when an action                 |
|          |         | returns `Nothing`.                                     |
+----------+---------+--------------------------------------------------------+
| IO       | Strict  | Do not allow extraction from IO type.                  |
+----------+---------+--------------------------------------------------------+
| ST       | Strict/ | Embed an opaque mutable data                           |
|          | Lazy    | Do not allow extraction of the data                    |
+----------+---------+--------------------------------------------------------+

+-------------------------------------------------------------------------------------------------+
| State sharing monads (defined in `transformers` package)                                        |
+-------------------------------------------------------------------------------------------------+
| The core semantics of the bind operation is to pass a shared state from one                     |
| operation to the next.                                                                          |
+--------+-------------+---------------+-----------------------------+----------------------------+
| Monad  | Transformer | mtl typeclass | Description                 | Typeclass operations       |
|        | Monad       |               |                             |                            |
+--------+-------------+---------------+-----------------------------+----------------------------+
| Reader | ReaderT     | MonadReader   | Read shared state           | ask, local, reader         |
+--------+-------------+---------------+-----------------------------+----------------------------+
| Writer | WriterT     | MonadWriter   | Write to shared state       | tell, pass, writer, listen |
+--------+-------------+---------------+-----------------------------+----------------------------+
| State  | StateT      | MonadState    | Read and write shared state | get, put, state            |
+--------+-------------+---------------+-----------------------------+----------------------------+
| RWS    | RWST        | MonadRWS      | All of the above            | All of the above           |
+--------+-------------+---------------+-----------------------------+----------------------------+

+---------------------------------------+
| Operations on monadic actions         |
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

Examples::

  StateT String []

  StateT is the outer monad and [] is the inner monad.


Transformers
------------

A monad can be wrapped inside another, which can in turn be wrapped inside
another forming a stack of monads. When we are in a current monad `m` we can
apply an adaptor called `lift` to adapt operations from a monad lower in the
stack to run in the current monad. A monad which can apply such an adaptation
via `lift` is called a monad transformer. This stack of monads each of which is
a transformer is called a monad transformer stack.

IO Monad has a special treatment and its own `MonadIO` typeclass for any monad
to lift operations from the IO monad. `MonadBase` typeclass generalizes that
and allows lifting operations from a base monad `b` to the current monad.
`MonadTrans` typeclass provides the ability to lift from one to monad to
another. `MonadBase` uses `MonadTrans` to apply multiple lifts throught the
transformer stack to lift from the base monad.

+-----------------------------------------------------------------------------+
| Summary of lifting operations in a transformer stack                        |
+-------------------+--------------+------------------------------------------+
| Typeclass         | Operations   | Description                              |
+===================+==============+==========================================+
| MonadIO           | liftIO       | lift a computation from the IO monad     |
+-------------------+--------------+------------------------------------------+
| MonadBase         | liftBase     | lift a computation from the base monad   |
+-------------------+--------------+------------------------------------------+
| MonadTrans        | lift         | lift from the argument monad to the      |
|                   |              | result monad                             |
+-------------------+--------------+------------------------------------------+
| MonadTransControl | liftWith,    | lift carrying the state of current monad |
|                   | restoreT     | restoreT can restore the state.          |
+-------------------+--------------+------------------------------------------+
| MonadBaseControl  | liftBaseWith,| lift base with state                     |
|                   | restoreM     |                                          |
+-------------------+--------------+------------------------------------------+

::

   --------------
  |  n (MonadIO) |
   --------------    ^
  |  m (MonadIO) |   |
   --------------  ^ |
                   | |
                   | | liftIO
   --------------  - -
  |  IO          |
   --------------

`lift` lifts operations in one monad to another. Note that the implementation
of `lift` is specfic to the two monads::

   -----------------
  |  t (MonadTrans) |  ^
   -----------------   | lift
  |  m              |
   -----------------

`liftBase` applies the necessary lifts through the stack to lift the base monad
to the current monad::

   ----------------
  |  n (MonadBase) |
   ----------------    ^
  |  m (MonadBase) |   |
   ----------------  ^ |
                     | |
                     | | liftBase (lift . lift ...)
   ----------------  - -
  |  b             |
   ----------------

`liftWith` captures the state of the current monad `t` before lifting.  It then
provides the m computation with a `Run` function that allows running `t n`
computations in `n` (for all `n`) on the captured state.  `restoreT` constructs
a `t` computation from the monadic state of `t` that is returned from a `Run`
function::

   ------------------------
  |  t (MonadTransControl) |  ^
   ------------------------   | liftWith (run m with captured state of t)
   ------------------------   | restoreT (restore the captured state of t)
  |  m                     |
   ------------------------

liftBaseWith is similar to liftBase but with captured state. restoreM restores
the state returned after running the computation in base::

   ------------------------
  |  n (MonadBaseControl)  |    ^
   ------------------------     |
  |  m (MonadBaseControl)  |  ^ |
   ------------------------   | |
                              | |
                              | | liftBaseWith (run b with captured state of m)
   ------------------------   _ _ restoreM (restore the captured state of m)
  |  b                     |
   ------------------------

Examples::

  f :: (MonadIO m) => ...
  res <- liftIO putStrLn "Hello"

  f :: (MonadBase m) => ...
  res <- liftBase baseOperation

  f :: (MonadBaseControl m) => ...
  runInBase <- liftBaseWith $ \run -> return (void . run)
  runInBase $ baseOperation

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

References
-----------

* https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html
* https://hackage.haskell.org/package/safe-exceptions
* https://github.com/fpco/safe-exceptions/blob/master/COOKBOOK.md
