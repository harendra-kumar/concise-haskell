Haskell in Computing
--------------------

+------------+--------------------------------------------+-------------+
| data input | slice, dice, transform, combine            | data output |
+------------+--------------------------------------------+-------------+
|            | Haskell                                    |             |
|            | (Concise, Composable, Correct & Efficient) |             |
+------------+--------------------------------------------+-------------+

Haskell - Executive Summary
---------------------------

Higher level qualities build upon lower level features. The key is providing a
feature without compromising on other features. For example C could be efficient,
parallel and distributed but can it be expressive, composable and ensure correctness at
the same time?

+---------------------------------------------------------------------------------------------------------+
| Money                                                                                                   |
+------------+------------+------------------+-----------------+-----------------+------------------------+
| Building cost           | Opportunity cost | Sustenance cost | Deployment cost | Suitability            |
+------------+------------+------------------+-----------------+-----------------+------------------------+
| Total Time & Resources  | Clock time       | Quality         |                 | Scalability            |
+------------+------------+------------------+-----------------+-----------------+------------------------+
| Build, Understand & Change                 | Test            | Performance     |                        |
+------------+-------------------------------+-----------------+-----------------+------------------------+
| Expressive | Composable                    | Correct         | Efficient       | Parallel & Distributed |
+------------+-------------------------------+-----------------+-----------------+------------------------+

Express this table as rings with haskell (logo) at core.

Expressive -> BUC
Composable -> BUC, test, scale
Correct -> test
Efficient -> Performance
Parallel -> performance
Parallel -> Scale
Distributed -> Scale

BUC -> time, resources, clock time
test -> clock time
test -> quality
Performance -> deployment cost
Scale -> suitability

TBD: function diagram with arrows between related concepts

+-----------------------+-----------------------------------------------------+
| Denotational          | Functional, pure                                    |
+-----------------------+-----------------------------------------------------+
| Correct               | Strong Static typing, equational reasoning          |
|                       | (denotational)                                      |
+-----------------------+-----------------------------------------------------+
| Expressive            | Polymorphism (higher order functions, typeclasses), |
|                       | Abstraction ladder, Lazy                            |
+-----------------------+-----------------------------------------------------+
| Composable            | Functional, Pure, lazy and Denotational             |
+-----------------------+-----------------------------------------------------+
| Parallel & Concurrent | Pure                                                |
+-----------------------+-----------------------------------------------------+

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

Product life cycle
------------------

+------------+----------------+
| Short term | Long term      |
+------------+----------------+
| Survival   | Growth         |
+------------+----------------+
| Protoype   | Mature Product |
+------------+----------------+
| Quick & dirty | Complex, Scalable, Performant, Sustainable |
+------------+----------------+
| Haskell all the way   |
+------------+----------------+

References: expressive, correctness, composable, efficiency
