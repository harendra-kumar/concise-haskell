Overview
--------

Two tracks, normal and exception.

Failure Representation
----------------------

+--------+---------+------------------------------------+
| Maybe  | Nothing | Error (only indication)            |
|        +---------+------------------------------------+
|        | Just    | Return value                       |
+--------+---------+------------------------------------+
| Either | Left    | Error value                        |
|        +---------+------------------------------------+
|        | Right   | Return value                       |
+--------+---------+------------------------------------+

Exceptions
----------

+--------------+--------------------------------------------------------------+
| Synchronous  | internal exception events generated from within a piece of   |
|              | code                                                         |
+--------------+--------------------------------------------------------------+
| Asynchronous | external exception events generated from outside a piece of  |
|              | code. For example, `threadKill`.                             |
+--------------+--------------------------------------------------------------+

Synchronous exceptions are thrown at well-defined places in the code and
supposed to be caught in the outer parts of the code. We can expect exceptions
from the inner code when are evaluating that code from the outer code.

Asynchronous exception are beyond the control of the currently running code;
they can arrive at any time whenever the external actors wish to throw them at
you.

Data (Representation)
---------------------

An exception is represented by a user defined algebraic data type. You can
encode arbitrary information available at the exception site in the exception
and throw it. At the catch site this information can be used by the handler.

An exception data type is dynamically typed i.e. an instance of ``Typeable``.
Use ``fromException`` and ``toException`` to convert a generic exception
(``SomeException``) to a specific type and vice-versa. These functions have a
default implementation in the typeclass ``Exception`` of which every exception
is made an instance of.

``Exception`` class also provides a ``displayException`` function to show the
exception in a human-friendly manner when it is caught, also ``Show`` is a
superclass of ``Exception`` so each exception is required to have a ``Show``
instance as well.  Example?

Control.Exception in base

::

  SomeException (Exception)
      IOException
      ArithException
      ...
      SomeAsyncException
      AsyncException
      NonTermination
      NestedAtomically
      ...

Control (Throw and Catch)
-------------------------

Control.Exception in base

There are two control tracks, normal and exceptional. Normally your run on the
regular track but if an error condition is detected we ``throw`` the train onto
the exception track. ``catch`` defines how to run on the exception track.
``try`` runs the train and tells you finally which track you are on, normal or
exception. Then you can decide how what to do on that track.

Handle (Catch) Exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------+------------------------------+---------------------------+
| just collect | ``try <action>``             | ``IO (Either e a)``       |
+--------------+------------------------------+---------------------------+
| handle it    | ``catch <action> <handler>`` | ``IO a``                  |
+--------------+------------------------------+---------------------------+

`catch` masks all asynchronous exceptions while `try` does not. Therefore
`catch` is suitable for asynchronous exceptions while `try` is more suitable
for synchronous exceptions.

Automatic cleanup: ``finally``, ``bracket`` or ``onException``.

unhandled exceptions are caught by the runtime, it prints the exception and
terminates the thread. When there are multiple threads only the thread that
received the exception is terminated.

Throw Exceptions
~~~~~~~~~~~~~~~~

Mechanisms used to indicate an error e.g. error or throw are untyped and
therefore can be used in an expression of any type without a type error.

Exceptions may be thrown from purely functional code, but may only be caught
within the 'IO' monad.

+---------------------------------------+--------------+----------------------+
| Exception e                           | Sync/Async   | Pure/IO              |
+---------------------------------------+--------------+----------------------+
| ``throw :: Exception e => e -> a``    | synchronous  | pure                 |
+---------------------------------------+              +----------------------+
| ``throwIO :: e -> IO a``              |              | IO                   |
+---------------------------------------+              |                      |
| ``ioError :: IOError -> IO a``        |              |                      |
+---------------------------------------+--------------+                      |
| ``throwTo :: ThreadId -> e -> IO ()`` | asynchronous |                      |
+---------------------------------------+--------------+----------------------+

Some Standard Exception APIs
----------------------------

Wildcard
~~~~~~~~

These can be used in any code pure, IO or any other monad. The type is ``a``

+-----------------------------------------------------------------------------+
| error - the wildcard exception                                              |
+======================+======================================================+
| error :: String -> a | error is completely untyped, and can be called from  |
|                      | anywhere. Throws an exception with a message.        |
+----------------------+-------------+----------------------------------------+

* throw
  see ``throwIO`` manual for the subtle difference between throw and throwIO.

Laziness can make exceptions manifest when the expression is evaluated:

::

  main = do
      -- throw (ExitFailure 5)
      x <- return $ toInteger $ div 1 0
      putStrLn "hello"
      y <- return (x + 2)
      putStrLn "hello"
      putStrLn $ show y -- div-by-zero exception is thrown here

IO Code
~~~~~~~

These can be used only in IO code. The type is ``IO a``

IO Exit Codes
^^^^^^^^^^^^^

::

  Module System.Exit
    exitFailure -- throws exception of type ExitCode
    exitSuccess
    exitWith
    die :: String -> IO a - like error but in IO.

IO Errors
^^^^^^^^^

* System.IO.Error
* type IOError = IOException

Monadic Error Handling
----------------------

MonadFail
~~~~~~~~~

+------------------+-------------+--------------------------------------------+
| Aborting monadic | fail        |                                            |
| computations     | (MonadFail) |                                            |
+------------------+-------------+--------------------------------------------+

Transformers
~~~~~~~~~~~~

+-------------+----------------------+------------+---------------------------+
| Transformer | Equivalent pure type | mtl Class  | Operations                |
+=============+======================+============+===========================+
| MaybeT      | Maybe                | -          |                           |
+-------------+----------------------+------------+---------------------------+
| ExceptT     | Either               | MonadError | throwError, catchError    |
+-------------+----------------------+------------+---------------------------+

Extensible Exceptions
~~~~~~~~~~~~~~~~~~~~~

These can be used in any monad implementing ``MonadThrow``. The type is ``m a``

See monad transformers chapter.

CallStack, Source location
--------------------------

References
----------

