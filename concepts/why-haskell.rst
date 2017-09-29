Why Haskell?
------------

+-----------------------+-----------------------------------------------------+
| High level feature    | Low level enabler of high level feature             |
+=====+=================+=====================================================+
| DRY | Expressive      | Polymorphism (higher order functions, typeclasses), |
|     |                 | Incremental abstraction ladder (low to high), Lazy  |
|     +-----------------+-----------------------------------------------------+
|     | Composable      | Denotational (Functional, Pure), lazy,              |
|     |                 | Composable concurrency                              |
+-----+-----------------+-----------------------------------------------------+
| Reasoning             | Sequential reasoning (purity), equational reasoning |
|                       | (denotational semantics)                            |
+-----------------------+-----------------------------------------------------+
| Correct               | Strong Static typing, purity enables better testing |
+-----------------------+-----------------------------------------------------+
| Efficient             | Compiled, Interesting compiler optimizations        |
|                       | possible due to purity                              |
+-----------------------+-----------------------------------------------------+

What is unique about Haskell?
-----------------------------

The first level debate is about Functional vs Object Oriented Programming (OOP)
languages. The proof of functional being a very important programming paradigm
is in the trends in traditional programming languages racing towards
introducing more and more functional features. Objective C has been replaced by
Swift and Java is being replaced by Scala both of which are designed to have
very strong functional features. Even traditional imperative languages like
Java and C++ too are introducing functional features. Even python has
introduced several functional features. If functional is not equal or better
than OOP why would OOP languages even bother about introducing it? In my
opinion, functional is going to dominate the OOP paradigm in coming future.

The second level debate is on static typing or dynamic typing (e.g.  Haskell vs
Clojure). Dynamic typing relies on actually executing all code paths to find
all bugs while static typing finds them right away at compile time. In any
reasonably sized project you would want the static typing paradigm otherwise
the cost of testing and fixing problems later would just become enormous and
unmanageable. All serious large scale programming languages are statically
typed e.g. Swift, Scala, OCaml, F#, Java, C++.

Assuming these debates are settled we can argue about what does Haskell bring
to the table differentiating it from other statically typed functional
languages like F#, OCaml, Scala or Swift for example. All these languages are
quite similar and have roots in OCaml the strict cousin of Haskell from the ML
family. Scala and Swift ride on the legacy of Objective C and JVM ecosystem
compatibility respectively. They have a hybrid OOP and functional paradigm
which confuses programmers and leaves a big scope of messing up your code
because of a confused design. On the other hand only functional languages have
a clean design providing only one (functional) way of thinking and designing.

Haskell has a clean design - no obligation of supporting a legacy. However, it
can support the JVM if you want to integrate with existing Java code. Other
than using a single functional only paradigm Haskell also has two unique design
features viz. purity and laziness. The immense value that purity brings to the
table is the power of sequential and equational reasoning which no other
language provides. This is something that is not easily appreciated easily but
is the most important aspect of Haskell. This is what brings order to the
chaos. It makes each and every part of the program to look like a sequential
flow rather than a mess of cyclic dependencies in which you spend most of your
time as a programmer to find out where a particular value is being changed.  Or
to sort out the mess of entangled control flow and trying to make sense out of
it.  Adele Goldberg aptly said, "In Smalltalk, everything happens somewhere
else." Haskell enforces does not allow you to have that mess to begin with by
design. The whole program is a directed acyclic graph except localized
recursion. Haskell builds a beautiful waterfall of data flow instead of a mess
of entangled wires. A Haskell program is just a series of transforms on data,
the data is handed over from one function to another. It does not hide data
under the carpet where someone sneakily comes and changes and you have no clue
who did it.

I was a fan of object oriented programming until I encountered the better
functional programming paradigm. OOP tried to improve upon the existing free
for all in C but did not solve the problem completely. In OOP, the fundamental
problem of cyclic data flow dependencies across objects remains. To make any
program scalable to understand by an ordinary programmer it needs to be
sequential in data flow. If you have to jump from one place to another and keep
a huge context in mind then it will make an ordinary head spin. There may be a
few extra ordinarily gifted minds which can make sense out of the chaos but an
average programmer will find it difficult to wade through it.

Think about refactoring an OOP program. Since it has state embedded in objects
and complex interactions, refactoring a complex program will be a nightmare.
However in Haskell, data is separated from code that works on it. It is much
easier to refactor both independently and because we have pure functions just
transforming data.

Edsger Dijkstra: “Object-oriented programming is an exceptionally bad idea
which could only have originated in California.”

Re-use is a myth in OO programming while it is a reality in Haskell because of
separation of data and code.

To appreciate the importance of sequential reasoning of human mind think about
understanding a piece of code sprinkled with gotos all over. Why was goto
discouraged in C in the first place? Why did we invent control flow structures?
Haskell just takes it to the next level and removes gotos in your design.

To solve this problem in a fundamental, language enforced manner Haskell
provides sequential and equational reasoning possible because of its pure
functional model.

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

* Another way to reuse
* TBD

Correctness
-----------

* Strong static typing
* Automatic memory management
* Equational reasoning due to purity
* Easier testing due to purity (quickcheck)

Concurrency
-----------

See the concurrency chapter

Parallelism
-----------

See the concurrency chapter

Reasoning
---------

* Sequential reasoning
* Human mind naturally thinks sequentially
* The amount of memory is limited
* Abstractions allow you to forget and reduce the scope
* provable, pure abstractions are key
* Pure transformations allow you to think sequentially, purely in terms of data and
  transformations on it.
* In OOP everything is mingled, you have hidden state in objects, you need to
  keep that in mind to understand how the operations behave. In pure functional the
  state is always explicit.
* OOP is messed easily, its not easy to mess pure functional, at least the
  purity cannot be compromised.
* Dependencies across objects could be real hell and cyclic making your head
  spin. Some languages have sorted out dependencies across packages but as long
  as impurity exists dependencies across units within a package will remain
  cyclic creating problems in understanding.

References
----------

* https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
* https://www.microsoft.com/en-us/research/publication/beautiful-concurrency/
* https://www.fpcomplete.com/blog/2016/11/mastering-time-to-market-haskell
* https://www.fpcomplete.com/blog/2016/11/comparison-scala-and-haskell

* http://raganwald.com/2010/12/01/oop-practiced-backwards-is-poo.html
* http://blog.jot.fm/2010/08/26/ten-things-i-hate-about-object-oriented-programming/comment-page-2/
* http://www.yegor256.com/2016/08/15/what-is-wrong-object-oriented-programming.html

* http://www.youscience.com/what-is-sequential-reasoning-and-why-does-it-matter/
* http://www.erasmatazz.com/library/the-mind/history-of-thinking/033d91601d1f41bdbb9d/verbal-reasoning.html
