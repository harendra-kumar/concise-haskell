Basic Concurrency
-----------------

GHC Options
-----------

-threaded

For real parallelism:

Concurrency
-----------

* Composable concurrency (purity)
* Tools: threadscope

Creating Threads
----------------

* Control.Concurrent (base package)

::

  forkIO :: IO () -> IO ThreadId
  killThread :: ThreadId -> IO ()
  throwTo :: Exception e => ThreadId -> e -> IO ()

async
~~~~~

The main additional functionality it provides is the ability to wait for the
return value of a thread

data Async a | represents an asynchronous IO action that will return a value of
type a, or die with an exception.::

  async :: IO a -> IO (Async a)
  withAsync :: IO a -> (Async a -> IO b) -> IO b

  wait :: Async a -> IO a
  waitAny :: [Async a] -> IO (Async a, a)
  poll :: Async a -> IO (Maybe (Either SomeException a))
  cancel :: Async a -> IO ()

  race :: IO a -> IO b -> IO (Either a b)
  concurrently :: IO a -> IO b -> IO (a, b)
  mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)

  newtype Concurrently a

  A value of type Concurrently a is an IO operation that can be composed with
  other Concurrently values, using the Applicative and Alternative instances.

  Calling runConcurrently on a value of type Concurrently a will execute the IO
  operations it contains concurrently, before delivering the result of type a.

Data Sharing/Communication/Synchronization
------------------------------------------

Synchronization Needs:

* Transactional read and modification of shared data (raced)
* Serialized read and modification with blocking wait (ordered)
* Send and receive data (one at a time or FIFO Channels)
* Binary Semaphore
* Quantity Semaphore

Facilities:

* MVar (fairness, single-wakeup, performance)
* stm (Composable atomicity, Composable blocking, robust error handling)

* Asynchronous exceptions
* Completely safe to interrupt threads because of purity

+--------------+------------------------------------+-------------------------+
| data IORef a | * mutable variable                 | new, read, write        |
|              | * CPU can reorder reads/writes     |                         |
+--------------+------------------------------------+-------------------------+
|              | * reads and writes are serialized  | atomicModify            |
|              | * Acts as a barrier to reordering  |                         |
+--------------+------------------------------------+-------------------------+
| data MVar a  | * mutable variable with explicit   | new, take, put          |
|              |   synch                            |                         |
|              | * Fair, single-wakeup, lazy,       |                         |
|              |   ordered                          |                         |
+--------------+------------------------------------+-------------------------+
| data Chan a  | * Unbounded FIFO channel           | new, dup, read, write   |
|              | * implemented with MVars           |                         |
+--------------+------------------------------------+-------------------------+
| data QSem    | * Quantity Semaphore: aqcuired and | new, wait, signal       |
|              |                                    |                         |
| data QSemN   |   released in units of one         |                         |
+--------------+------------------------------------+-------------------------+

MVars
-----

* As synchronized mutable variables ::

    A <- take, modify (B waits), put
    B <- take, modify (A waits), put

* As a binary semaphore MVar (), with takeMVar and putMVar as wait and signal. ::

    A <- take, do something (B waits), put
    B <- take, do something (A waits), put

* As channels, with takeMVar and putMVar as receive and send, and ::

    A puts -> B takes
    A puts -> B takes
    ...
    A blocks if full
    B blocks if empty

they are very simple and susceptible to race conditions, deadlocks or uncaught
exceptions. STM is more sophisticated and easier to get right than MVars.
Unless you need low level control and control over performance overhead use
STM.

Fairness: No thread can be blocked indefinitely on an MVar unless another
thread holds that MVar indefinitely.

Laziness: MVars are lazy; if you place an expensive unevaluated thunk inside an
MVar, it will be evaluated by the thread that consumes it, not the thread that
produced it.

Memory Ordering: MVar operations are always observed to take place in the order
they are written in the program

stm
---

STM: A monad supporting atomic memory transactions. Provides Alternative and
MonadPlus instances as well.

Either the whole transaction happens or nothing. TVars can be read and modified
inside the atomically blocks to communicate across threads::

  atomically :: STM a -> IO a

Retry execution of the current memory transaction because it has seen values in
TVars which mean that it should not continue (e.g. the TVars represent a shared
buffer that is now empty). The implementation may block the thread until one of
the TVars that it has read from has been udpated::

  retry :: STM a

+-----------------+--------------------------------+--------------------------+
| data TVar a     | Mutable variable in STM        | new, read, write         |
+-----------------+--------------------------------+--------------------------+
| data TMVar a    | Mutable variable with synch in | new, take, put           |
|                 | STM                            |                          |
+-----------------+--------------------------------+--------------------------+
| data TChan a    | Unbounded FIFO channel         | new, dup, clone, peek,   |
|                 |                                | read, write              |
+-----------------+--------------------------------+--------------------------+
| data TQueue a   | Unbounded FIFO (Faster,        | new, peek, read, write   |
|                 | no dup or clone                |                          |
+-----------------+--------------------------------+--------------------------+
| data TBQueue a  | Bounded FIFO                   | new, peek, read, write   |
+-----------------+--------------------------------+--------------------------+
| data TSem a     | Quantity semaphore: acquired   | new, wait, signal        |
|                 | and released in units of one.  |                          |
|                 | No fairness.                   |                          |
+-----------------+--------------------------------+--------------------------+
| data TArray i e | Mutable array with MArray      |                          |
|                 | interface in STM               |                          |
+-----------------+--------------------------------+--------------------------+

parallel
--------

* Purity
* Determinism

  * Program does the same thing but faster
  * No trade-off with correctness
  * No race conditions or deadlocks

* basic pure parallelism: sparks & strategies

  * Control.Parallel.Strategies
  * Eval monad (rpar/rseq)

    * deterministic parallelism
    * minimal control over the evaluation order
  * Strategies

    * Adding parallelism over pure (lazy) data structures
    * Composability: combine Strategies into larger ones
    * modular: (e `using` s) parallelism separate from algorithm
    * myList `using` parList rdeepseq
  * Lazy evaluation is the magic ingredient that bestows
    modularity, and thus forms the basis of Strategies. Programmer aware of:

    * Evaluation order (rpar requires lazy computation)
    * garbage collection (result of rpar must not be discarded)

  * The Par monad (does not require laziness)
* parallel
* accelerate (GPU programming)

sparks::

  par :: a -> b -> b
  a `par` b is exactly equivalent semantically to b.

Eval monad::

  runEval :: Eval a -> a

  myStrat :: Strategy (a,b)
  myStrat (a,b) = do { a' <- rpar a; b' <- rseq b; return (a',b') }
  myStrat (a,b) = (,) <$> rpar a <*> rseq b

Deterministic, modular, and compositional parallelism strategies::

  type Strategy a = a -> Eval a
  using :: a -> Strategy a -> a
  withStrategy :: Strategy a -> a -> a
  dot :: Strategy a -> Strategy a -> Strategy a -- compose

  Strategies: r0/rseq/rdeepseq/rpar/rparWith
  Startegies for traversable/lists/tuples
  Strategic function application

Distributed
-----------

* transient
* cloud-haskell

References
----------

* http://community.haskell.org/~simonmar/
* https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/concurrent-haskell.pdf Concurrent Haskell - Paper
* http://simonmar.github.io/bib/papers/stm.pdf Composable Memory Transactions
* https://www.microsoft.com/en-us/research/publication/beautiful-concurrency/ STM
* https://www.microsoft.com/en-us/research/publication/tackling-awkward-squad-monadic-inputoutput-concurrency-exceptions-foreign-language-calls-haskell/?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fmarktoberdorf%2F Tackling the Awkward Squad
* https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
* http://chimera.labs.oreilly.com/books/1230000000929 Parallel and Concurrent Programming in Haskell By Simon Marlow
