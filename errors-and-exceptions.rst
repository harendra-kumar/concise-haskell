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

MonadFail
---------

+------------------+-------------+--------------------------------------------+
| Aborting monadic | fail        |                                            |
| computations     | (MonadFail) |                                            |
+------------------+-------------+--------------------------------------------+

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
from the inner code when are invoking it from the outer code.

Asynchronous exception are beyond the control of the currently running code
they can arrive at any time whenever the external actors wish to throw them at
you.

Data
----

An exception is represented by a user defined algebraic data type. You can
encode arbitrary information available at the exception site in the exception
and throw it. At the catch site this information can be used by the handler.

An exception data type is dynamically typed i.e. an instance of Typeable.  Use
fromException and toException to convert a generic exception (SomeException) to
a specific type and vice-versa. These functions have a default implementation
in the typeclass Exception of which every exception is made an instance of.

Exception class also provides a displayException function to show the exception
in a human-friendly manner when it is caught, also Show is a superclass of
Exception so each exception is required to have a Show instance as well.
Example?

Control.Exception in base

SomeException (Exception)
    IOException
    ArithException
    ...
    SomeAsyncException
    AsyncException
    NonTermination
    NestedAtomically
    ...

Control
-------

Control.Exception in base

Handle Exceptions
~~~~~~~~~~~~~~~~~

+--------------+------------------------------+---------------------------+
| just collect | ``try <action>``             | ``IO (Either e a)``       |
+--------------+------------------------------+---------------------------+
| handle it    | ``catch <action> <handler>`` | ``IO a``                  |
+--------------+------------------------------+---------------------------+

`catch` masks all asynchronous exceptions while `try` does not. Therefore
`catch` is suitable for asynchronous exceptions while `try` is more suitable
for synchronous exceptions.

cleanup: finally, bracket or onException.

Throw Exceptions
~~~~~~~~~~~~~~~~

Mechanisms used to indicate an error e.g. error or throw are untyped and
therefore can be used in an expression of any type without a type error.

* throw
* throwIO (e -> IO a), ioError (IOError -> IO a)
* throwTo - throw to a thread

Some Standard Exception APIs
----------------------------

error - the wildcard exception
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+-------------+----------------------------------------+
| error :: String -> a | error is completely untyped, and can be called from  |
|                      | anywhere. Throws an exception with a message.        |
+----------------------+-------------+----------------------------------------+

IO Exceptions
~~~~~~~~~~~~~

These can be used in IO code. The type is ``IO a``

IO Exit Codes
^^^^^^^^^^^^^

System.Exit
exitFailure -- throws exception of type ExitFailure
exitSuccess
exitWith
die :: String -> IO a - like error but in IO.

IO Errors
^^^^^^^^^

* System.IO.Error
* type IOError = IOException

Extensible Exceptions
---------------------

See monad transformers chapter.

CallStack, Source location
--------------------------

References
----------

