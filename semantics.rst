Denotational & Operational Semantics
====================================

Terminology
-----------

+----------------------------+------------------------------------------------+
| Denotation                 | What an expression denotes or means            |
+----------------------------+------------------------------------------------+
| Bottom (_|_)               | * An undefined or non-existing value           |
|                            | * The ``undefined`` function in Haskell        |
+----------------------------+------------------------------------------------+
| Expression                 | A Haskell expression is a combination of       |
|                            | functions and values                           |
|                            | e.g. ``(square ((1 + 2) * 3)) + 4``            |
|                            | Note: ``+`` and ``*`` are infix functions      |
+----------------------------+------------------------------------------------+
| Reduction                  | Applying functions in an expression to reduce  |
|                            | it to a shorter expression                     |
+----------------------------+------------------------------------------------+
| Evaluation                 | Operational term for reduction                 |
+----------------------------+------------------------------------------------+
| Redex                      | An expression which can be reduced further     |
+----------------------------+------------------------------------------------+
| Weak Head Normal Form      | An expression whose outermost part (the head)  |
| (WHNF)                     | is not a function application (it can be a     |
|                            | constructor though).                           |
+----------------------------+------------------------------------------------+
| Normal Form (NF)           | An expression without redexes                  |
+----------------------------+------------------------------------------------+
| Reduced Normal Form (RNF)  | Same as normal form                            |
+----------------------------+------------------------------------------------+
| Strict reduction           | Inside out (bottom up) reduction of an         |
|                            | expression                                     |
+----------------------------+------------------------------------------------+
| Non-Strict reduction       | Outside in (top down) reduction of an          |
|                            | expression                                     |
+----------------------------+------------------------------------------------+
| Lazy evaluation            | An implementation of non-strict reduction      |
+----------------------------+------------------------------------------------+
| Eager evaluation           | An implementation of strict reduction          |
+----------------------------+------------------------------------------------+
| call-by-need               | lazy evaluation                                |
+----------------------------+------------------------------------------------+

Language Features
-----------------

+----------------------+--------------------------+---------------------------+
| Equational Reasoning | Denotational Semantics   | Referential Transparency  |
+----------------------+--------------------------+---------------------------+
| Expressive Power     | Infinite data structures | Non-strict semantics      |
+----------------------+                          |                           |
| Modularity           |                          |                           |
+----------------------+--------------------------+---------------------------+

Language Design Modularity
--------------------------

+----------------------------+----------------------------+
| Pure Functional (Haskell)  | Other languages            |
+============================+============================+
| Denotational semantics     | Mixed semantics            |
+----------------------------+                            |
| Operational semantics      |                            |
+----------------------------+----------------------------+

Denotational vs Operational Semantics
-------------------------------------

+------------------------------------+----------------------------------------+
| Denotational                       | Operational                            |
+====================================+========================================+
| What a program does                | How it does it                         |
+------------------------------------+----------------------------------------+
| Specification                      | Implementation & Execution             |
+------------------------------------+----------------------------------------+
| Reason about the meaning and       | Reason about performance               |
| correctness                        |                                        |
+------------------------------------+----------------------------------------+
| Equational reasoning for           | Space leaks, Evaluation                |
| correctness                        | Strategies, Garbage collection         |
+------------------------------------+----------------------------------------+

Haskell Expressions
-------------------

+----------------------------------------+
| Expression                             |
+----------------------+-----------------+
| Function application | Value           |
+----------------------+------+----------+
|                      | Data | Function |
+----------------------+------+----------+

Haskell Denotational Semantics
------------------------------

Equational Reasoning
~~~~~~~~~~~~~~~~~~~~

Referentially Transparent Expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Pure functional programming has a property called referential transparency or
immutability.

No value ever changes after it is born.
Arguments to a function are effectively passed by value. A function cannot
implicitly make any changes to any of its environment. No globals, pointers
etc. A function does not do anything other than what is visible from its
signature or arguments.

This allows us to use a function in any environment without any fear of
changing the meaning of the program in unintended ways.

The only way a function is connected to the rest of the world is via its
published interface i.e. the arguments. So a function can be plucked out of a
place and used somewhere else.

Special Denotations
^^^^^^^^^^^^^^^^^^^

Role of Bottom

* non-termination
* Recursion
* Infinite data structures
* undefined values (lazy eval)

+----------------------------+------------------------------------------------+
| f x = x * x                |                                                |
+----------------------------+------------------------------------------------+
| f x = f (x + 1)            | Infinite loop                                  |
+----------------------------+------------------------------------------------+
| let x = x in x             |                                                |
+----------------------------+------------------------------------------------+

We say that ⊥ is the completely "undefined" value or function. Every basic data
type like Integer or () contains one ⊥ besides their usual elements. So the
possible values of type Integer are
{\displaystyle \bot ,0,+1,-1,+2,-2,+3,-3,\dots } \bot
,0,+1,-1,+2,-2,+3,-3,\dots

Adding ⊥ to the set of values is also called lifting.

As another example, the type () with only one element actually has two
inhabitants:
{\displaystyle \bot ,()} \bot ,()

Now, {\displaystyle \bot } \bot  (bottom type) gives us the possibility to
denote partial functions:
{\displaystyle f(n)={\begin{cases}1&{\mbox{ if }}n{\mbox{ is }}0\\-2&{\mbox{ if }}n{\mbox{ is }}1\\\bot &{\mbox{ else }}\end{cases}}} f(n)={\begin{cases}1&{\mbox{ if }}n{\mbox{ is }}0\\-2&{\mbox{ if }}n{\mbox{ is }}1\\\bot &{\mbox{ else }}\end{cases}}

Partial order picture with bottom at the bottom.

Reasoning By Substitution
^^^^^^^^^^^^^^^^^^^^^^^^^

Ulitmately what do we get from referential transparency (purity) and
denotational semantics? Equational reasoning.

http://www.haskellforall.com/2013/12/equational-reasoning.html
http://neilmitchell.blogspot.in/2015/02/refactoring-with-equational-reasoning.html

A Haskell program is purely a set of equations. Each function definiton is a
set of equations and those equations then expand to another set of equations.

expression A = expression B

Where expression A could be a function definition or a pattern match at top
level or in a let or where binding inside a function.

Thanks to referential transparency, we can freely substitute a term by its
equivalent equation without worrying about any side effects. This works just
like mathematical equations. By way of substitution we can prove equivalence of
two expressions.

Non-Strict Reduction Semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Haskell reduces expressions in a non-strict manner                          |
+-----------------------------------------------------------------------------+

TODO: We need a picture of an expression here.

::

  fst (square (1 + 2), square 3)

  f a b c = case a > b of
    True -> c
    False -> 1

  f (1 + 2) (3 * 4) (12 / 2)


Non-strict semantics require an expression to be reduced in an outside-in
fashion or in a top down fashion if the expression is represented as a tree
with root on top. Outside-in reduction of an expression ensures that a
subexpression will `never` be computed if it is not used in the expression.

How will (A or B) where A and B are redexes and we will know whether they are
TRUE or FALSE only after evaluating them. Which one gets evaluated first?

Default semantics:

* pattern matching is strict by default (WHNF)
* case expression

Strict and Non-strict functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| A function which always needs an argument (technically in WHNF) is called   |
| strict in that argument.                                                    |
+-----------------------------------------------------------------------------+
| Compiler has an option to use eager instead of lazy evaluation when it      |
| knows the function is strict in a certain argument.                         |
| Notice that `non-strict does not necessarily mean lazy` in this case. GHC   |
| performs a strictness analysis to detect this and may deploy eager          |
| evaluation.                                                                 |
+-----------------------------------------------------------------------------+
| Strictness condition                                                        |
+-----------------------------------+-----------------------------------------+
| f is strict in first argument iff | ``f _|_ a = _|_``                       |
+-----------------------------------+-----------------------------------------+
| ``id x = x``                                                                |
+-----------------------------------------------------------------------------+
| ``fst (a, b) = a -- strict in first argument``                              |
+-----------------------------------------------------------------------------+
| Non-strict functions                                                        |
+-----------------------------------------------------------------------------+
| A function which discards an argument is called non-strict in that argument.|
| GHC implements this using lazy evaluation to honor non-strict semantics.    |
+-----------------------------------------------------------------------------+
| ``fst (a, b) = a -- non-strict in second argument``                         |
+-----------------------------------------------------------------------------+

Haskell Operational Semantics
-----------------------------

Lazy evaluation
~~~~~~~~~~~~~~~

Graph Reduction

The value is represented by a thunk or closure, which is code which knows how
to compute the value. When the value is needed this code is executed and the
value is generated for the consumer.

Lazy evaluation as fine grained data flow programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions are low level pipes.

Infinite data structures are like the indefinite data producers. Lazy
evaluation composes multiple functions together and a data element passes
through those functions like data passes through pipes. The usual design model
is to let the data pass through all stages quickly and then get garbage
collected and not hold up the data. If you hold up it turns into a space leak.

Each function is a pipe connected to another function, another pipe. Lazy
evaluation drives the data through this big chain of pipes. The pipes get
activated when we want to draw something from the remote end. This is literally
data driven programming.

We need a picture here with functions/closures connected to each other and how
data flows through them. How the whole evaluation machinery is cranked to
generate output from input.

Haskell lazy evaluation is fundamentally data driven or flow based programming,
or in other words stream based programming.

Stream fusion basically fuses multiple stages of lazy evaluation into one
big imperative like loop instead of chain invocation of each stage. Same way we
represent loops by pipes.

Within a high level pipe we can use strict evaluation rather than lazy
evaluation and fuse that whole logic together to make it more efficient.

Can we automatically make everything strict rather than lazy depending on
feasibility?

Controlling Strictness
~~~~~~~~~~~~~~~~~~~~~~

The language has to respect non-strict semantics, however where it does not
impact the behavior of the program we can choose strict evaluation.

* bang patterns
* strict by default extension

Understanding a Haskell Program
-------------------------------

An imperative mind runs a program in the head line by line. A lazy Haskell mind
composes a program in the head. When reading Haskell do not try to run each
statement then and there, just think that this is being composed and then it
will be run in the required order when needed. It might get composed further or
transformed and then composed to create a bigger composition. Just keep your
mind lazy!  This is perhaps the hardest part for an imperatively trained mind.
