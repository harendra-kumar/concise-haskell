Functionality Features
----------------------

This needs a lot of work. The judgement here is subjective and may not be
accurate. The parameters need to be defined more specifically and need to
provide more details on specific features and other reasons.

Statically Typed
~~~~~~~~~~~~~~~~

+----------+----------------------+------------------------+------------------+
| Language | Expressive           | Composable             | Correct          |
+==========+======================+========================+==================+
| Haskell  | Excellent (lazy)     | Excellent (lazy, pure) | Safe             |
+----------+----------------------+------------------------+------------------+
| Rust     | ?                    | Good?                  | Safe             |
+----------+----------------------+------------------------+------------------+
| Swift    | Good                 | Depends                | Safe             |
|          |                      | (OO + Functional)      |                  |
+----------+----------------------+------------------------+------------------+
| Scala    | Good                 | Depends                | Safe             |
|          |                      | (OO + Functional)      |                  |
+----------+----------------------+------------------------+------------------+
| C        | Poor                 | Poor                   | Unsafe           |
+----------+----------------------+------------------------+------------------+
| C++      | Average              | Poor                   | Unsafe           |
+----------+----------------------+------------------------+------------------+
| Go       | Average?             | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+
| Java     | Average              | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+

Dynamically Typed
~~~~~~~~~~~~~~~~~

+----------+----------------------+------------------------+------------------+
| Language | Expressive           | Composable             | Correct          |
+==========+======================+========================+==================+
| Clojure  |                      | Good                   | Safe             |
+----------+----------------------+------------------------+------------------+
| Erlang   |                      | Good                   | Safe             |
+----------+----------------------+------------------------+------------------+
| Python   |                      | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+

Performance Features
--------------------

* Concurrency, parallelism and distributed computing features are a combination
  of language features and libraries.
* Composable concurrency and parallelism?

+----------+------------+------------+---------------------+------------------+
| Language | Efficient  | Concurrent | Parallel            | Distributed      |
+==========+============+============+=====================+==================+
| Haskell  | Fast       | base       | parallel            |                  |
|          |            | stm        | DPH                 | cloud-haskell    |
|          |            | async      |                     | courier          |
|          |            | haxl       |                     | hdph             |
|          |            | hactor     |                     | transient        |
|          |            | future     |                     |                  |
|          |            | promise    |                     |                  |
+----------+------------+------------+---------------------+------------------+
| Rust     | Very Fast  | Threads    |                     |                  |
+----------+------------+------------+---------------------+------------------+
| Swift    | Very Fast  | GCD        |                     |                  |
+----------+------------+------------+---------------------+------------------+
| Scala    | Fast       | threading  | Parallel Collections| Akka             |
|          |            | Futures    |                     |                  |
|          |            | Actors     |                     |                  |
|          |            | STM        |                     |                  |
|          |            | Akka       |                     |                  |
+----------+------------+------------+---------------------+------------------+
| C        | Very Fast  |            |                     |                  |
+----------+------------+------------+---------------------+------------------+
| C++      | Very Fast  |            |                     |                  |
+----------+------------+------------+---------------------+------------------+
| Go       | Fast       | Goroutines |                     |                  |
+----------+------------+------------+---------------------+------------------+
| Java     | Fast       | threading  |                     |                  |
|          |            | Futures    |                     |                  |
|          |            | ForkJoin   |                     |                  |
+----------+------------+------------+---------------------+------------------+

Misc Notes
----------

* Portability

  * Haskell - Native, JVM, interoperates with C/R/Java/Python/Rust

* Swift: compile time ARC, unsafe (unmanaged memory) is possible.

  * live coding playgrounds
  * REPL
  * JIT in dev
  * ABI compatible with C
* Rust: Ownership and borrowing for shared memory and concurrency

  * unsafe (unmanaged memory) is possible.
