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

Error Handling
--------------

These are not commonly used. The usual way to handle errors is the exception
handling described next.

+------------------+-------------+--------------------------------------------+
| Unconditional    | error       | error is untyped, can be called from       |
| termination      |             | anywhere                                   |
+------------------+-------------+--------------------------------------------+
| Aborting monadic | fail        |                                            |
| computations     | (MonadFail) |                                            |
+------------------+-------------+--------------------------------------------+
|                  | Maybe       |                                            |
+------------------+-------------+--------------------------------------------+
|                  | Either      |                                            |
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

Control.Exception in base

SomeException
  Exception
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

* throw
* throwIO (e -> IO a), ioError (IOError -> IO a)
* throwTo - throw to a thread

IO Errors
---------

* System.IO.Error
* type IOError = IOException

Extensible Exceptions
---------------------

See monad transformers chapter.

References
----------

