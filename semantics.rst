Denotational & Operational Semantics
====================================

Terminology
-----------

+------------------------+----------------------------------------------------+
| Expression             | A Haskell expression is a combination of           |
|                        | functions and values                               |
|                        | e.g. ``(square ((1 + 2) * 3)) + 4``                |
|                        | Note: ``+`` and ``*`` are infix functions          |
+------------------------+----------------------------------------------------+
| Denotation             | Meaning of a term or expression                    |
+------------------------+----------------------------------------------------+
| Denotational semantics | Each term or expression has a well defined         |
|                        | denotation or meaning, in some universe of         |
|                        | mathematical meanings.                             |
+------------------------+----------------------------------------------------+
| Side effect            | A change in environment (global state, IO)         |
|                        | effected by a function                             |
+------------------------+----------------------------------------------------+
| Pure function          | A function which always returns the same output    |
|                        | for identical inputs. This implies that the        |
|                        | function has no side effect.                       |
+------------------------+----------------------------------------------------+
| Pure functional        | A language in which all functions are pure, not    |
|                        | allowing mutation of data structures               |
+------------------------+----------------------------------------------------+
| Immutability           | No data structure can ever be modified or mutated. |
+------------------------+----------------------------------------------------+
| Referential            | any variable can be replaced with its actual value |
| transparency           | at any point of execution (because the variable can|
|                        | never change)                                      |
+------------------------+----------------------------------------------------+
| Equational reasoning   | Reasoning about a program just like mathematical   |
|                        | equations by substituting terms with their         |
|                        | definitions.                                       |
+------------------------+----------------------------------------------------+
| Reduction              | Applying functions in an expression to reduce      |
|                        | it to a shorter expression                         |
+------------------------+----------------------------------------------------+
| Evaluation             | Operational equivalent for reduction               |
+------------------------+----------------------------------------------------+
| Redex                  | An expression which can be reduced further         |
+------------------------+----------------------------------------------------+
| Weak Head Normal Form  | An expression whose outermost part (the head)      |
| (WHNF)                 | is not a function application (it can be a         |
|                        | constructor though).                               |
+------------------------+----------------------------------------------------+
| Normal Form (NF)       | An expression without redexes                      |
+------------------------+----------------------------------------------------+
| Reduced Normal Form    | Same as normal form                                |
| (RNF)                  |                                                    |
+------------------------+----------------------------------------------------+
| Strict reduction       | Inside out (bottom up) reduction of an             |
|                        | expression                                         |
+------------------------+----------------------------------------------------+
| Non-Strict reduction   | Outside in (top down) reduction of an              |
|                        | expression                                         |
+------------------------+----------------------------------------------------+
| Lazy evaluation        | An implementation of non-strict reduction          |
+------------------------+----------------------------------------------------+
| Eager evaluation       | An implementation of strict reduction              |
+------------------------+----------------------------------------------------+
| Call-by-need           | lazy evaluation                                    |
+------------------------+----------------------------------------------------+
| Partially ordered set  |                                                    |
| (poset)                |                                                    |
+------------------------+----------------------------------------------------+
| Semantic approximation |                                                    |
| order                  |                                                    |
+------------------------+----------------------------------------------------+
| Lazy value             | All ordinary Haskell values are lazy meaning they  |
|                        | are evaluated on demand. Lazy values are also      |
|                        | called lifted values.                              |
+------------------------+----------------------------------------------------+
| Diverging computation  | A function or computation that does not return to  |
|                        | the caller is said to diverge. Divergence is       |
|                        | denoted by bottom.                                 |
+------------------------+----------------------------------------------------+
|                        | In order theory, the least element (if it exists)  |
|                        | of a partially ordered set is also called the      |
|                        | bottom and is denoted by ⊥.                        |
|                        +----------------------------------------------------+
| Bottom (_|_)           | In Haskell, bottom is the least defined value added|
|                        | to all types to denote undefined, diverging and    |
|                        | lazy values.                                       |
+------------------------+----------------------------------------------------+
| Lifting                | Adding a `bottom` to an ordered set is called      |
|                        | `lifting`.                                         |
+------------------------+----------------------------------------------------+


Language Features
-----------------

+----------------------+--------------------------+---------------------------+
| User level feature   | Language feature         | Underlying enabler        |
+======================+==========================+===========================+
| Equational Reasoning | Denotational Semantics   | Referential Transparency  |
+----------------------+--------------------------+---------------------------+
| Expressive Power     | Infinite data structures | Non-strict semantics      |
+----------------------+                          |                           |
| Modularity           |                          |                           |
+----------------------+--------------------------+---------------------------+

Language Design Modularity
--------------------------

+------------------------------+----------------------------+
| Haskell (or pure functional) | Other languages            |
+==============================+============================+
| Denotational semantics       | Operational is inseparable |
+------------------------------+                            |
| Operational semantics        |                            |
+------------------------------+----------------------------+

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
| Equational reasoning for           | Space leaks, Optimal evaluation,       |
| correctness                        | Garbage collection                     |
+------------------------------------+----------------------------------------+

Denotational Semantics
----------------------

Constructing mathematical objects (called denotations) that describe the
meanings of expressions from the programming language. An important tenet of
denotational semantics is that semantics should be compositional: the
denotation of a program phrase should be built out of the denotations of its
subphrases.

Referential Transparency
~~~~~~~~~~~~~~~~~~~~~~~~

Pure functional programming has a property called referential transparency or
immutability. All expressions n Haskell are referentially transparent.

No value ever changes after it is born.  Arguments to a function are
effectively passed by value. A function is completely isolated from its
environment i.e. it cannot implicitly make any changes to any of its
environment. No globals, pointers etc. A function does not do anything other
than what is visible from its signature or arguments.

The only way a function is connected to the rest of the program is via its
published interface i.e. the arguments. So a function can be plucked out of a
place and used somewhere else.

This allows us to use a function in any environment without any fear of
changing the meaning of the program in unintended ways.

Bottom & Lifting
~~~~~~~~~~~~~~~~

Bottom does not have an explicit denotation in the language but is an implicit
concept to understand the meaning of several language constructs in
mathematical terms:

* non-termination
* Recursion
* Infinite data structures
* undefined values (lazy eval)

Non-Strict Reduction
~~~~~~~~~~~~~~~~~~~~

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

How does (A OR B) get evaluated where A and B are redexes? We will know whether
they are TRUE or FALSE only after evaluating them. Which one gets evaluated
first?

Default semantics:

* pattern matching in case (and function definitions) is strict (WHNF)
* However pattern matching in let and where is lazy
* strict pattern match is the only way to strictness

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

Equational Reasoning
~~~~~~~~~~~~~~~~~~~~

Reasoning by substitution.

Ulitmately what do we get from referential transparency (purity) and
denotational semantics? Ability to easily reason about or understand how a
program works. Equational reasoning.

A Haskell program is nothing but a set of equations. Each function definiton is
a set of equations which expand to other set of equations and so on.

expression A = expression B

Where expression A could be a function definition at top
level or in a let or where binding inside a function.

Thanks to referential transparency, we can freely substitute a term by its
equivalent equation without worrying about any side effects. This works just
like mathematical equations. By way of substitution we can prove equivalence of
two expressions.

Operational Semantics
---------------------

Lazy Evaluation
~~~~~~~~~~~~~~~

Graph Reduction

An unevaluated value is represented by a thunk or closure, which is code which
knows how to compute the value. When the value is needed this code is executed
and the value is generated for the consumer.

Program execution is driven by IO, statements are not executed unless they are
needed by a computation driven by IO. There is no sequential evaluation of all
statements in the control flow path.

Controlling Evaluation
~~~~~~~~~~~~~~~~~~~~~~

The language has to respect non-strict semantics, however when it does not
impact the semantics of the program we can choose strict evaluation.

* bang patterns
* strict by default extension

Understanding a Haskell Program
-------------------------------

An imperative mind runs a program in the head line by line. On the other hand,
a lazy Haskell mind composes a program in the head. When reading Haskell do not
try to run each statement then and there, just think that this is being
composed and then it will be run in the required order when needed. It might
get composed further or transformed and then composed to create a bigger
composition. Just keep your mind lazy!  This is perhaps the hardest part for an
imperatively trained mind.

References
----------

* https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/informal-semantics.html
* http://www.haskellforall.com/2013/12/equational-reasoning.html
* http://neilmitchell.blogspot.in/2015/02/refactoring-with-equational-reasoning.html
