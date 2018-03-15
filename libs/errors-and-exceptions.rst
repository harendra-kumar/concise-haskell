Overview
--------

Two tracks, normal and exception. An exception just short circuits the
execution and makes the control flow jump directly to the exception handler.

Checked vs Unchecked Exceptions
-------------------------------

Checked errors are explicitly typed in the signature. Unchecked errors are more
of an independently mechanism that is implicitly attached to a function and you
never know, it may or may not be there, the type system does not help in
knowing the presence of errors, you need to know about them and deal with them.
Any function which is in IO monad or which has a MonadThrow constraint can
throw many different type of errors. The onus is on the programmer to find out
about them all and deal with them if she wants to.

Maybe/Either/ExceptT approach is superior to throw/catch mechanism as it can
tell you specifically which exceptions can occur (checked) in a given function
and looking at the function signature you can tell that the function can fail.

throw/catch mechanism is primarily for runtime system to throw exceptions
without having to express that in the type signature of the function. That way
this is not a preferrable mechanism for programmer controlled error cases. Such
exceptions by the runtime system can be thrown in pure code (e.g. DivideByZero
or in IO code when some IO error occurs).

The programmer should always consider the possiblity that any function that
returns an IO type or a monad transformer that can be instantiated using the IO
monad (or using MonadThrow) may throw an exception of any type.

Handling Errors in Pure Code
----------------------------

There are two fundamental ways to represent error conditions.

+--------+---------+------------------------------------+
| Maybe  | Nothing | Error (only indication)            |
|        +---------+------------------------------------+
|        | Just    | Return value                       |
+--------+---------+------------------------------------+
| Either | Left    | Error value                        |
|        +---------+------------------------------------+
|        | Right   | Return value                       |
+--------+---------+------------------------------------+

Maybe is used when you only one error case it only tells whether there is an
error or no error. When you want to distinguish between error cases use Either.

These allow the programmer to explicitly and fully control the error handling.
In addition to these ``throw`` is another way to express an exception
condition.

Maybe and Either as Monads
--------------------------

We can also use Maybe and Either as monads to short circuit computations in a
do block. TBD provide examples.

+-------------+---------------------------------------------------------------+
| Monad       | Operations                                                    |
+=============+===============================================================+
| Maybe       | Nothing value short circuits the sequence.                    |
+-------------+---------------------------------------------------------------+
| Either      | Left value short circuits the sequence.                       |
+-------------+---------------------------------------------------------------+
| Except      | throwE, catchE                                                |
+-------------+---------------------------------------------------------------+
|             | runExcept :: Except e a -> Either e a                         |
+-------------+---------------------------------------------------------------+

Maybe, Either and Except provide short circuiting behavior very much like the
"return" statements in the imperative languages.

Handling Errors in IO code: Exceptions
--------------------------------------

+--------------+--------------------------------------------------------------+
| Synchronous  | internal exception events generated from within a piece of   |
|              | code                                                         |
+--------------+--------------------------------------------------------------+
| Asynchronous | external exception events generated from outside a piece of  |
|              | code. For example, `threadKill`.                             |
+--------------+--------------------------------------------------------------+

Synchronous exceptions are internal or inherent to the code being executed,
they come from within the code being run under an exception handler. The
programmer runs a piece code under an exception handler and if the code throws
an expcetion it is caught by that handler. Therefore synchronous expcetions do
not interrupt the flow of code arbitrarily.

On the other hand asynchronous exceptions are not inherent to the code being
run they come from outside, they are caused by external factors beyond the
control of the currently running code, for example some other thread may use
threadKill to kill the current thread. Such exceptions can arrive at any point
of time in the code flow. For example we may have opened a file handle and
reading from the file and the exception arrives even before we cloe the file
handle. Therefore asynchronous exceptions can interfere with the code flow.
They are beyond the control of the currently running code.

Data: Representing an Exception
-------------------------------

An exception is represented by a user defined algebraic data type. You can
encode arbitrary information available at the exception site in the exception
and throw it. At the catch site this information can be used by the handler.

An exception data type is dynamically typed i.e. an instance of ``Typeable``.
Every exception must be an instance of ``Exception`` typeclass implementing
``fromException`` and ``toException``. Use these to convert a generic exception
(``SomeException``) to a specific type and vice-versa. These functions have a
default implementation.

``Exception`` typeclass also provides a ``displayException`` function to show
the exception in a human-friendly manner when it is caught, also ``Show`` is a
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

There are two control tracks in a program, normal and exceptional. Normally
your run on the regular track but if an error condition is detected we
``throw`` the train onto the exception track. ``catch`` defines what the
exception track is and automatically puts it on the exception track when an
exception occurs.  ``try`` runs the train and tells you finally which track you
are on, normal or exception. Then you can decide how what to do on that track.

Masking Asynchronous exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Asynchronous exceptions can arrive at any time without notice. To perform
certain operations safely we may have to block async exceptions from occuring
until we are ready to handle to them.

+-------------------+---------------------------------------------------------+
| ``mask``          | mask/restore async exceptions                           |
+-------------------+---------------------------------------------------------+
| ``interruptible`` | Allow asynchronous exceptions to be raised even inside  |
|                   | mask.                                                   |
+-------------------+---------------------------------------------------------+

Handle IO Exceptions (synchronous)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``try`` is simpler to use, it is just like calling a function that returns an
``Either`` value. If you are interested in only synchronous exceptions this is
the simplest option.  However, it is inherently unsafe for handling
asynchronous exceptions as exception handling is actually performed outside the
try block where there is no exception handler installed; if asynchronous
exceptions arrive at that time we will have no handler installed.

It just transforms a function to return an Either, collecting any IO exceptions
in the ``Left`` case and return value in the ``Right`` case.

+------------------------------+---------------------------+
| ``try <action>``             | ``IO (Either e a)``       |
+------------------------------+---------------------------+

Handle IO Exceptions (synchronous and asynchronous)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``catch`` is safe for asynchronous exceptions as it safely installs the handler
and only then runs the IO action guaranteeing that any exception arriving
during the execution will be caught.

+------------------------------+---------------------------+
| ``catch <action> <handler>`` | ``IO a``                  |
+------------------------------+---------------------------+

Automatic resource cleanup: ``finally``, ``bracket`` or ``onException``.

Unhandled Exceptions
~~~~~~~~~~~~~~~~~~~~

Unhandled exceptions are caught by the runtime system, it prints the exception
and terminates the thread. When there are multiple threads only the thread that
received the exception is terminated.

Throw IO Exceptions
~~~~~~~~~~~~~~~~~~~

Mechanisms used to indicate an error e.g. ``error`` and ``throw`` are untyped
and therefore can be used in an expression of any type without a type error.

Exceptions may be thrown from purely functional code, but may only be caught
within the 'IO' monad.

+---------------------------------------+--------------+----------------------+
| Exception e                           | Sync/Async   | Pure/IO              |
+---------------------------------------+--------------+----------------------+
| ``throw :: Exception e => e -> a``    | synchronous  | pure/IO              |
+---------------------------------------+              +----------------------+
| ``throwIO :: e -> IO a``              |              | IO                   |
+---------------------------------------+              |                      |
| ``ioError :: IOError -> IO a``        |              |                      |
+---------------------------------------+--------------+                      |
| ``throwTo :: ThreadId -> e -> IO ()`` | asynchronous |                      |
+---------------------------------------+--------------+----------------------+

Exceptions in Pure Expressions
------------------------------

``evaluate`` evaluates and uncovers exceptions from a pure expression. Notice
that it returns a result in the IO monad so that we can handle the exceptions
using the exception catching mechanisms in the IO monad::

  evaluate :: a -> IO a

Laziness and Exceptions
~~~~~~~~~~~~~~~~~~~~~~~

Laziness can make exceptions manifest when the expression is evaluated:

::

  main = do
      -- throw (ExitFailure 5)
      x <- return $ toInteger $ div 1 0
      putStrLn "hello"
      y <- return (x + 2)
      putStrLn "hello"
      putStrLn $ show y -- div-by-zero exception is thrown here

Some Standard Exception APIs
----------------------------

Wildcard/untyped exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These can be used in any code pure, IO or any other monad. The type is ``a``

+-----------------------------------------------------------------------------+
| error - the wildcard exception                                              |
+======================+======================================================+
| error :: String -> a | error is completely untyped, and can be called from  |
|                      | anywhere. Throws an exception with a message.        |
+----------------------+-------------+----------------------------------------+

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

Equivalents of Maybe and Either for monadic code. They are purely synchronous
in nature and are completely independent of the IO exceptions and
try/catch/throw.

+-------------+----------------------+------------+---------------------------+
| Transformer | Equivalent pure type | mtl Class  | Operations                |
+=============+======================+============+===========================+
| MaybeT      | Maybe                |            |                           |
+-------------+----------------------+------------+---------------------------+
| EitherT     | Either               |            |                           |
+-------------+----------------------+------------+---------------------------+
| ExceptT     | Either               | MonadError | throwError, catchError    |
+-------------+----------------------+------------+---------------------------+

MaybeT, EitherT and ExceptT provide short circuiting behavior very much like
the "return" statements in the imperative languages. TODO - Explain this with
examples of parallels between the two paradigms.

Extensible Exceptions
~~~~~~~~~~~~~~~~~~~~~

Extension of try/catch/throw for any monad, not just IO.

These can be used in any monad implementing ``MonadThrow``. The type is ``m a``

See monad transformers chapter.

MonadThrow errors can be conveniently cast into Maybe and Either. You don't
need a ``try`` you can just interpret it as an ``Either`` value::

  instance MonadThrow Maybe where
    throwM _ = Nothing

::

  Prelude Path Control.Monad.Catch> f = throwM (InvalidAbsDir "x") :: MonadThrow m => m Int
  Prelude Path Control.Monad.Catch> f :: Maybe Int
  Nothing

CallStack, Source location
--------------------------

Which exception mechanism to use?
---------------------------------

One good thing about the throw/catch mechanism is that we can catch any and all
types of exceptions we can define our own exception types anywhere. All of
these percolate up and get collected by a catching mechanism. However in case
of ExceptT we fix the type of errors by parameterizing the ExceptT with a given
type. If someone wants to define a different type the the exceptT error type
has to be a made a sum type and the new error has to be added to it. In
contrast throw is more modular as we can always define and throw new types of
exceptions.

The disadvantage of throw/catch is that there can be indeterminate types of
errors that can be thrown and we cannot be sure whether we caught them all or
there is something lurking under that will come under a catchall case.

Checked Errors
~~~~~~~~~~~~~~

* Pure

  * Maybe
  * Either

* Monadic

  * MaybeT
  * ExceptT (it is in fact EitherT)

Unchecked Exceptions
~~~~~~~~~~~~~~~~~~~~

* Pure

  * throw
  * evaluate and then try/catch

* Monadic (IO/runtime exceptions)

  * throw/throwIO
  * try (in absence of async exceptions)
  * throwTo (async throw)
  * mask (mask async exceptions)
  * catch (for sync/async exceptions)

* Monadic (extensible/monad transformers)

  * MonadMask/MonadCatch/MonadThrow
  * throwM
  * try
  * mask
  * catch

Design Pattern
~~~~~~~~~~~~~~

Simple rule: Use checked errors where the whole program is in your control and
therefore you can always modify the error type to add more cases when you want.
Use unchecked errors where the pieces of code you are throwing the errors from
is independent of where the errors may be caught.

The unchecked exception mechanism allows you to define a new exception anywhere
and throw it. The good thing about it is that it makes the code modular, the
bad thing about it is that the onus is on the catcher to catch and interpret
all errors, there is always a possibility that the catcher might miss some. So
the modularity does not come without a cost.

The unchecked mechanism is useful when you really don't care about those errors
in most cases but sometimes you may you leave the decision on the user, so you
do not want the inconvenience of handling it always. The user has a choice to
use a catchall for catching all such errors and put them in just one basket or
even let the runtime system deal with them. When you always care  and want to
user to explicitly know about the errors then use a checked mechanism.

Example
~~~~~~~

Consider a case where you are designing an API for creating a user in a
database. When creating a user you may find that a user with the same name
already exists. But there are many other kind of errors that can occur e.g. the
database connection failed or your thread got killed or some other kind of IO
error happened beacuse of which your requrest to the database failed.

There are multiple ways you can design this API. For example::

    createUser :: UserRecord -> IO ()

The case when a user already exists is also conveyed via throwIO. In this case
the signature does not convey any error condition and the user of this API has
to know the presence of errors and use ``try`` to catch the exceptions. This
includes the important case of the user already existing which is really not an
exceptional condition it is an inherent part of the API and we would always
want the user to handle it unconditionally. It is not a good idea to leave it
to the user. However, all other error conditions are in a different category
they are not part of the inherent business logic for this API, they are just
other possibilities always lurking around and can happen for this API or for
others as well.

Consider another possibility of the API design::

    createUser :: MonadThrow m => UserRecord -> m ()

This is equivalent to the previous one except that the monad is generalized and
the signature reminds the user that it can throw an exception which is implicit
in case of IO. But that's not an improvement.

Now consider this one::

    createUser :: UserRecord -> m (Maybe ())

This API captures the API's inherent error in the signature and leaves the
general, really exceptional conditions to the unchecked exception mechanism.
This is the one you should go with.

References
----------

* http://hackage.haskell.org/package/exceptions
* http://hackage.haskell.org/package/safe-exceptions
* https://hackage.haskell.org/package/safe-exceptions-checked-0.1.0/docs/Control-Exception-Safe-Checked.html
* http://hackage.haskell.org/package/resourcet

* https://github.com/fpco/safe-exceptions#readme
* https://haskell-lang.org/tutorial/exception-safety
* https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices
* https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
* https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
* https://www.yesodweb.com/blog/2014/05/exceptions-cont-monads
* https://www.well-typed.com/blog/2015/07/checked-exceptions/
