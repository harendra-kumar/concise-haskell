Functionality Features
----------------------

Statically Typed
~~~~~~~~~~~~~~~~

+----------+----------------------+------------------------+------------------+
| Language | Expressive           | Composable             | Correct          |
+==========+======================+========================+==================+
| Haskell  | Excellent (lazy)     | Excellent (lazy, pure) | Extra Safe       |
+----------+----------------------+------------------------+------------------+
| Rust     |                      | Medium?                | Safe             |
+----------+----------------------+------------------------+------------------+
| Swift    | Strong               | Flexible               | Safe             |
+----------+----------------------+------------------------+------------------+
| Scala    | Strong               | Flexible               | Safe             |
+----------+----------------------+------------------------+------------------+
| C        | Poor                 | Poor                   | Unsafe           |
+----------+----------------------+------------------------+------------------+
| C++      | Medium               | Poor                   | Unsafe           |
+----------+----------------------+------------------------+------------------+
| Go       | Medium?              | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+
| Java     | Medium               | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+

Dynamically Typed
~~~~~~~~~~~~~~~~~

+----------+----------------------+------------------------+------------------+
| Language | Expressive           | Composable             | Correct          |
+==========+======================+========================+==================+
| Clojure  |                      | Medium                 | Safe             |
+----------+----------------------+------------------------+------------------+
| Erlang   |                      | Medium                 | Safe             |
+----------+----------------------+------------------------+------------------+
| Python   |                      | Poor                   | Safe             |
+----------+----------------------+------------------------+------------------+

Performance Features
--------------------

* Concurrency, parallelism and distributed computing features are a combination
  of language and library features.
* Composable concurrency and parallelism?

+----------+------------+------------+---------------------+------------------+
| Language | Efficient  | Concurrent | Parallel            | Distributed      |
+==========+============+============+=====================+==================+
| Haskell  | Fast       | base       | parallel            | Static Pointers  |
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
* Rust: Ownership and borrowing for shared memory anc concurrency

  * unsafe (unmanaged memory) is possible.
