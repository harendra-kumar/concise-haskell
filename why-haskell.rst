Why Haskell?
------------

+-----------------------+-----------------------------------------------------+
| High level feature    | Low level enabler of high level feature             |
+=======================+=====================================================+
| Expressive            | Polymorphism (higher order functions, typeclasses), |
|                       | Incremental abstraction ladder (low to high), Lazy  |
+-----------------------+-----------------------------------------------------+
| Composable            | Denotational (Functional, Pure), lazy               |
+-----------------------+-----------------------------------------------------+
| Correct               | Strong Static typing, equational reasoning          |
|                       | (denotational), unique testing techniques           |
+-----------------------+-----------------------------------------------------+
| Efficient             | Compiled, Interesting compiler optimizations        |
+-----------------------+-----------------------------------------------------+
| Parallel              | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+
| Concurrent            | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+
| Distributed           | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+

Expressive Power: Abstractions & DSLs
-------------------------------------

Expressive power is directly related to ability to reuse. Haskell has very
powerful abstraction tools allowing maximum reuse.

* Low level abstraction facilities

  * parametric polymorphism
  * higher order functions
  * ad-hoc polymorphism
* Examples of some high level abstraction tools

  * Functors
  * Applicatives
  * Monads
* Haskell provides an incremental abstraction ladder where you can keep
  building higher level abstractions on top of lower level. You can create high
  level domain specific langauges to suit your application.

Composability
-------------

TBD

Correctness
-----------

* Strong static typing
* Automatic memory management
* Equational reasoning due to purity
* Easier testing due to purity (quickcheck)

Concurrency
-----------

* http://community.haskell.org/~simonmar/

* Basic Concurrency: base (Control.Concurrent)

  * forkIO
  * Mvar
  * Chan
  * Asynchronous exceptions
  * Completely safe to interrupt threads because of purity

* stm (Composable atomicity, Composable blocking, robust error handling)

  * Mvar (fairness, single-wakeup, performance)
* async
* haxl

Tools: threadscope

Parallel
--------

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

Distributed
-----------

* cloud-haskell
* transient

References
----------

* https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
* https://www.microsoft.com/en-us/research/publication/beautiful-concurrency/
* https://www.fpcomplete.com/blog/2016/11/mastering-time-to-market-haskell
* https://www.fpcomplete.com/blog/2016/11/comparison-scala-and-haskell
