Why Haskell?
------------

TBD: function diagram with arrows between related concepts
TBD: add examples to prove each point
TBD: add references

+-----------------------+-----------------------------------------------------+
| High level feature    | Low level feature                                   |
+=======================+=====================================================+
| Expressive            | Polymorphism (higher order functions, typeclasses), |
|                       | Abstraction stack (low to high), Lazy               |
+-----------------------+-----------------------------------------------------+
| Composable            | Denotational (Functional, Pure), lazy               |
+-----------------------+-----------------------------------------------------+
| Correct               | Strong Static typing, equational reasoning          |
|                       | (denotational)                                      |
+-----------------------+-----------------------------------------------------+
| Efficient             | Native, highly optimized code                       |
+-----------------------+-----------------------------------------------------+
| Parallel              | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+
| Concurrent            | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+
| Distributed           | Pure, Composable                                    |
+-----------------------+-----------------------------------------------------+

Expressive Power: Abstractions & DSLs
-------------------------------------

* Expressive power comes from reuse techniques
* Reuse by abstraction - parametric polymorphism
* ad-hoc polymorphism
* higher order functions
* e.g. functor family fo abstractions allows reuse of functions in different
  contexts in different ways.

* Haskell can be pretty low level with unlifted types and it could as high as
  possible with DSLs.
* The highest level abstractions are called DSLs

+-----------------+----------------------------------+
| Low level       | High Level                       |
+-----------------+----------------------------------+
| More Flexible   | Less flexible                    |
+-----------------+----------------------------------+
| Wider scope     | Narrower (domain specific) scope |
+-----------------+----------------------------------+
| Less expressive | More expressive                  |
+-----------------+----------------------------------+
| Verbose         | Concise                          |
+-----------------+----------------------------------+
| e.g. Functors   | e.g. Monads                      |
+-----------------+----------------------------------+
| e.g. threads    | e.g. async                       |
+-----------------+----------------------------------+

Challenges:

* Proliferation of too many abstractions can overwhelm and cause confusion,
  requires time to learn.
* You have to fit your problem into the right set of abstractions. So you have
  to know common abstractions and the ability to map your problem to those.
  That is when you take the full advantage of Haskell.
* How to find the right abstractions for your problem and how to be productive
  with them quickly?
* In contrast in other languages may have limited and simple abstractions and
  you can just use ad-hoc ways to solve your problem. However, using generics
  effectively could be as difficult as the abstractions in Haskell.
* You have to curate and choose wisely (this is where we need automation and
  innovation)

* quicksort is an example of expressive power
* Parallel mergesort in simon marlow's presentation is a good example of higher
  order functions and composable parallelism together.

Composability
-------------

Denotations are compositional, i.e. the meaning of a program like 1+9 only
depends on the meaning of its constituents:

It is one of the key properties of purely functional languages like Haskell
that a direct mathematical interpretation like "1+9 denotes 10" carries over to
functions, too: in essence, the denotation of a program of type Integer ->
Integer is a mathematical function {\displaystyle \mathbb {Z} \to \mathbb {Z} }
{\mathbb  {Z}}\to {\mathbb  {Z}} between integers.

Imperative languages are tightly tied to operational semantics which describes
their way of execution on a machine.
In contrast, the meaning of purely functional languages is by default
completely independent from their way of execution. The Haskell98 standard even
goes as far as to specify only Haskell's non-strict denotational semantics,
leaving open how to implement them.

In the end, denotational semantics enables us to develop formal proofs that
programs indeed do what we want them to do mathematically. Ironically, for
proving program properties in day-to-day Haskell, one can use Equational
reasoning, which transforms programs into equivalent ones without seeing much
of the underlying mathematical objects we are concentrating on in this chapter.
But the denotational semantics actually show up whenever we have to reason
about non-terminating programs, for instance in Infinite Lists.

Modular code by lazy evaluation:
minimum = head . sort
zip [0..] xs
prefix xs ys = and (zipWith (==) xs ys)

* rpar - concurrency modularity via laziness

* There is an interplay (trade-off) between modularity and efficiency in Haskell
  too. Sometimes you have to sacrifice modularity for performance.

Correctness
-----------

* strong static typing
* Automatic memory management
* Equational reasoning due to purity
* Easier testing due to purity

Downsides:

* Moldable clay vs rigid blocks
* Being always rigorously right could be taxing in rapidly changing environments
* Better tools can help reduce the type strictness tax
* GC pauses could be problematic in some cases (can be improved)

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

Benefits
--------

Building and maintenance cost => 100 people vs 10 people. The effect is
compounded by reduced communication overhead. A team of 10 will require a much
less communications overhead and thereofore much more productive.

References
----------

* https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
* https://www.microsoft.com/en-us/research/publication/beautiful-concurrency/
* https://www.fpcomplete.com/blog/2016/11/mastering-time-to-market-haskell
* https://www.fpcomplete.com/blog/2016/11/comparison-scala-and-haskell
