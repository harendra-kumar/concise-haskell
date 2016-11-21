Why Haskell?
------------

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

Expressive => High level, Haskell can be pretty low level with unlifted types
and it could as high as possible with DSLs.

Building cost => 100 people vs 10 people. The effect is compounded by reduced
communication overhead. A team of 10 will require a much less communications
overhead and thereofore much more productive.

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

There is an interplay (trade-off) between modularity and efficiency in Haskell
too.

Comparisons
-----------


References: expressive, correctness, composable, efficiency

Why not Haskell?
----------------

* Learning Functional
* Learning Abstractions
* Being always rigorously right could be taxing in rapidly changing environments

Learning Goals
--------------

* Zero to Functional & Haskell in 2 months
* Zero to Haskell in 1 month
