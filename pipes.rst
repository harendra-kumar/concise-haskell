Pipes
-----

Bidirectional Streaming Component
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A ``Proxy`` is the most general `open` component, with four openings:


::

  Proxy a' a b' b m r

  Upstream | Downstream
      +---------+
      |         |
  a' <==       <== b'  -- Information flowing upstream
      |         |
  a  ==>       ==> b   -- Information flowing downstream
      |    |    |
      +----|----+
           v
           r

Specialized Components
~~~~~~~~~~~~~~~~~~~~~~

Some openings of Proxy can be closed by using ``()`` on the input end or ``X``
(i.e. ``Void``) on the output end, to create more specialized components. In
the following text we will only be talking about downstream components:

+------------+-----------------+----------------+-----------------------------+
| Downstream | Proxy X () () b | Producer b m r | ``a  ==>       ==> |``      |
| Components +-----------------+----------------+-----------------------------+
|            | Proxy () a () b | Pipe a b m r   | ``a  ==>       ==> b``      |
|            +-----------------+----------------+-----------------------------+
|            | Proxy () a () X | Consumer a m r | ``|  ==>       ==> b``      |
+------------+-----------------+----------------+-----------------------------+

Consuming and Producing Actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will call a component with at least one opening an `open` component. An
`open` component with an ``output`` end can ``yield`` and one with an ``input``
end can ``await``:

+----------+-------------------+
| Producer | yield             |
+----------+-------------------+
| Pipe     | yield, await      |
+----------+-------------------+
| Consumer | await             |
+----------+-------------------+

Composing Components
~~~~~~~~~~~~~~~~~~~~

+-----------------------+---------+-------------------------------------------+
| A producing component | ``for`` | action consuming the output and producing |
|                       |         | some component                            |
+-----------------------+---------+-------------------------------------------+
| A producing action    | ``~>``  | action consuming the output and producing |
|                       |         | some component                            |
+-----------------------+---------+-------------------------------------------+
| Any component         | ``>~``  | A consuming component                     |
+-----------------------+---------+-------------------------------------------+
| A producing component | ``>->`` | A consuming component                     |
+-----------------------+---------+-------------------------------------------+

* ``cat``: a pipe that produces whatever it receives.

A Closed Component
~~~~~~~~~~~~~~~~~~

When a composition closes all the openings, it produces a `closed` component
called an ``Effect``. Ultimately our goal is to compose components to produce
an ``Effect``.

+----------+-----+----------+---+-------------+
| Producer | >-> | Consumer | = | Effect      |
+----------+-----+----------+---+-------------+

An ``Effect`` can be run using ``runEffect`` to produce a monadic action in
monad `m`::

  runEffect :: Monad m => Effect m r -> m r

ListT
~~~~~

::

  -- a Producer can be converted into a ListT (Select)
  -- and vice-versa (enumerate)
  newtype ListT m a = Select { enumerate :: Producer a m () }
