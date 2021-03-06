Operational Semantics: Evaluating Expressions
=============================================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

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
| Free variable          | x is a free variable in g                          |
|                        | ``f = \x -> let g = \y -> x + y in g x``           |
+------------------------+----------------------------------------------------+

Concepts and Implementations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------------------------+---------------------------------------+
| Conceptual Term                     | Specific Implementation Term          |
+=====================================+=======================================+
| Reduction                           | Evaluation                            |
+-------------------------------------+---------------------------------------+
| Non-strict Reduction                | Lazy Evaluation                       |
+-------------------------------------+---------------------------------------+
| Strict Reduction                    | Eager Evaluation                      |
+-------------------------------------+---------------------------------------+
| Abstraction                         | Programming                           |
+-------------------------------------+---------------------------------------+

Overview
--------

This chapter contains the operational details about the denotational aspects
that we discussed in the previous chapter.

Referential Transparency
------------------------

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

Bottom
------

Bottom does not have an explicit denotation in the language but is an implicit
concept to understand the meaning of several language constructs in
mathematical terms:

* non-termination
* Recursion
* Infinite data structures
* undefined values (lazy eval)

..
  Bottom - Denotation for special cases
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  Alegbraic data construction

  In a strict language, all constructors are strict by default, i.e. Just ⊥ = ⊥
  As a consequence, all domains of a strict language are flat.

  But in a non-strict language like Haskell, constructors are non-strict by
  default and Just ⊥ is a new element different from ⊥, because we can write a
  function that reacts differently to them:

  f (Just _) = 4
  f Nothing  = 7

  This gives rise to non-flat domains as depicted in the former graph. What
  should these be of use for?
  In the context of Graph Reduction, we may also think of ⊥ as an unevaluated
  expression. Thus, a value x = Just ⊥ may tell us that a computation (say a
  lookup) succeeded and is not Nothing, but that the true value has not been
  evaluated yet.

Lazy Evaluation
---------------

A function may or may not need its arguments at run-time, depending on its
implementation. However, we may not be able to determine that fact at compile
time.

In a lazy evaluation strategy, the evaluation decision is deferred to the
actual site where an argument is used inside the function and therefore it
happens at run time. The compiler must generate code in such a way that the
evaluation is triggered at run-time at the site where the argument is actually
used. If the argument is used at more than one places the compiler needs to
make sure that the evaluation happens once and then reused at other places.
This evaluation is based on the `run-time data dependencies` and not based on
the static structure of the expression.

For example, the `expression graph` of `fst (1, 2 + 3)` has `fst` depending on
both the arguments, whereas in the `real dependency graph`, `fst` has a
dependency only on the first argument and therefore we do not need to evaluate
`2 + 3`. But, in general, we can decide this only after we start evaluating
`fst`. So we need to generate the code such that we trigger the evaluation of
`2 + 3` at the place where it is used. However, in many cases, including `fst`
it can be determined at the compile time and GHC actually does that using
`strictness analysis` and no runtime mechanism is needed in those cases.

* Graph Reduction or lazy evaluation
* Need a picture showing bottom up and top down reduction paths in a tree

* http://www.well-typed.com/blog/2017/09/visualize-cbn Visualize lazy
  evaluation

Expressions and Data
~~~~~~~~~~~~~~~~~~~~

From program evaluation perspective, there are two important types of entities
in a Haskell program, expressions and data.  Expressions are unevaluated values
which eventually evaluate to a data constructor, data is represented by a data
constructor holding another data constructor or unevaluated expressions (WHNF).

Expressions may consist of data constructors or function applications. An
expression in general may represent a concrete data data type or an abstract
data type of some arity (a function).

Data specification consists of data constructors. Constructors are like slots
or labeled boxes or wrappers holding data.  The data they are holding could be
anything, an unevaluated expression or data.  We don't know what it is until we
open the box. The box is opened by doing a pattern match.

Expressions are reduced or evaluated to data constructors.  A data constructor
itself cannot be reduced or evaluated, it creates a data element on the heap.
However, the contents inside the data constructor can again be referring to
expressions which can be evaluated.

Operations on Expressions and Data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are mainly three types of operations during program execution. A pattern
match deconstructs data, a function application reduces an expression, a
constructor application creates new data.

+------------+-------------+----------------+---------------------------------+
| Entity     | Operation   | Trigger        | Output                          |
+============+=============+================+=================================+
| Expression | Function    | case scrutinee | Constructor                     |
|            | Application |                |                                 |
+------------+-------------+----------------+---------------------------------+
| Expression | Constructor | case scrutinee | Constructor                     |
|            | Application |                |                                 |
+------------+-------------+----------------+---------------------------------+
| Data       | Deconstruct | pattern match  | Expression or Constructor       |
+------------+-------------+----------------+---------------------------------+

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A function which always needs an argument (technically in WHNF) is called   |
| strict in that argument.                                                    |
+-----------------------------------------------------------------------------+
| GHC performs a strictness analysis to detect whether a function is always   |
| strict and may deploy eager evaluation when it is strict.                   |
+-----------------------------------------------------------------------------+
| Strictness check                                                            |
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

* A constructor is always lazy
* A single argument function is either lazy or strict in its argument.
* A multiple argument function is lazy or strict in an argument conditional on
  the values of other arguments. For example::

    f x y = if y > 10 then x + 1 else 1
    f x 1  -- does not need x
    f x 11 -- needs x

Evaluation
~~~~~~~~~~

A pattern match triggers evaluation of the expression we are matching on,
because it needs a constructor to pattern match on. That is as lazy as we can
get we can no longer procrastinate. Without the constructor there is no way we
can proceed.

Evaluation of an expression proceeds until it hits a constructor i.e. we are
looking at a box of data also called WHNF (Anyway its not possible to evaluate
further until we pattern match and know what is inside the box). The box is
then pried open by pattern matching on the constructors and the constituents
taken apart.  The case analysis then proceeds to perform the next pattern match
which will trigger another evaluation if we have an unevaluated expression
inside the box.

In essence the whole evaluation process is just a series of pattern matches and
we need to evaluate expressions to enable the pattern matches.  Thus, it is
a series of alternating pattern match and function applications i.e.
(pat+)(apply+).

This is how an evaluation of a case expression (`case analysis`) looks like in
general::

  expr =
    case (scrutinee expression) of
      pattern1 -> (output expression1)
      pattern2 -> (output expression2)
      ...

When we need `expr` in WHNF, its evaluation is started. The case statement
`scrutinizes` the expression which triggers its evaluation to WHNF.  Once we
have reach the outermost constructor we can pattern match. The pattern match
decides the path to take and then we need to evaluate the corresponding
`output expression` in WHNF.

Closures
^^^^^^^^

All heap objects are represented by a closure. A closure could be a `data
constructor`, a `function` or a `thunk`. A closure has a header and a payload.
The header has an info table and an entry code.

The entry code for the closure is usually the code that will evaluate the
closure. There is one exception: for functions, the entry code will apply the
function to the arguments given in registers or on the stack, according to the
calling convention. The entry code assumes all the arguments are present - to
apply a function to fewer arguments or to apply an unknown function, the
generic apply functions are used.

`Constructors`: The entry code for a constructor returns immediately to the
topmost stack frame, because the data constructor is already in WHNF. The
payload consists of the data constituents of the constructor.

`Functions:` Top level functions are represented by a static function closure
and the rest by a dynamic function closure. The payload of the function closure
contains the free variables of the function. A static closure has no payload,
because there are no free variables of a top-level function.

`Concrete values:` A thunk is a closure that represents an expression for a
concrete value. Top level expressions (not functions) are represented by static
thunks and rest by dynamic thunks. A static thunk is also known as a constant
applicative form, or CAF. A dynamic thunk payload contains the
free variables of the expression. A thunk differs from a function closure in
that it can be updated.

Closure Evaluation
^^^^^^^^^^^^^^^^^^

An expression's closure is entered when a pattern match wants to evaluate the
expression. The closure could be a function or a thunk, constructors require no
evaluation.

We are always in the context of some closure. The closure may save its
registers on the stack before it calls another closure. Because it needs to
pass parameters and the return address in registers.

In case of a top level function application the parameters are passed (in
registers), the return address in the parent closure is passed, and a call to
the closure to be evaluated is made. Once the evaluation to WHNF is done the
called closure makes a call to the return address. The called closure will
create a new closure for the return value which will be a constructor (WHNF).
The components of the constructor may be unevaluated closures.

In case of a dynamic function closure or thunk the free variables of the
expression are part of the closure structure. A thunk or dynamic function
closure is created by its parent closure. The parent closure inserts the
references to the closures of the free variables (evaluated or not) at the time
of its creation.

Garbage Collection & Space Leaks
--------------------------------

Explain how garbage collection works. For example, if we have to update the
last node of a list, what all will get garbage collected. Draw a picture.

Space Leaks
~~~~~~~~~~~

Lazy evaluation is like a streaming paradigm built into the language. It is
consumer driven, the whole machinery is cranked only when the last guy in the
chain demands a value.  Therefore, a value is produced only to be consumed
immediately by the next stage and so on until the effect propagates to the
final consumer.

Streaming is like pipes connected together and data (water) flowing from one
end of the system, through the network of pipes and then coming out of the
other end.  There can be some buffering zones in this network where water
(data) gets collected.  Such buffering zones should be minimal. When we design
the system in a way that we buffer too much of data in between without consuming
it then we might end up consuming too much memory, this is called a space leak.

To avoid space leaks we must design the program in such a way that we consume
the produced data as soon as possible.

Performance & Controlling Evaluation
------------------------------------

Fundamentally, the language has to respect non-strict semantics, however when
it does not impact the semantics of the program strict evaluation can be
employed.

* strictness analysis
* bang patterns
* strict by default extension

Understanding a Haskell Program
-------------------------------

When understanding a program a programmer evaluates the program in his/her
mind.  The difference in evaluation strategy can make a huge difference when
trying to understand a program. An imperatively trained mind runs a program in
the head line by line, sequentially. On the other hand, a lazily trained
Haskell mind has to compose a program in the head. If you are coming from an
imperative mindset, when reading Haskell do not try to run each statement then
and there, just think that this is being composed and then it will be run in
the required order when needed. It might get composed further or transformed
and then composed to create a bigger composition. Just keep your mind lazy!
This is perhaps the hardest part for an imperatively trained mind.

We need to understand the dependency relationships among the components of a
program.

Lazy vs Eager Evaluation
------------------------

Data Dependencies - Lazy vs Eager Evaluation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The high level difference in lazy evaluation and eager evaluation is that the
latter puts the burden of specifying data dependencies on the programmer while
the former infers it automatically.

In imperative languages dependencies among computations are specified by the
sequence of statements in the program.  The programmer has to think about
dependencies and encode them in the sequence of statements.  A statement
computing a data element must come before another statement using the data
element.  If the sequence of statements changes, the dependencies will change
and the meaning of the program will change.

A Haskell program figures out the dependencies among program elements
automatically, and the program execution is driven by these implicit dependency
relationships rather than the sequence of statements as written in the program.
The sequence of statements in the program is irrelevant.  However, the
dependencies and therefore the execution sequence can be explicitly controlled
when desired (e.g. IO Monad).

Eager Evaluation
~~~~~~~~~~~~~~~~

In eager evaluation strategy, everything is evaluated in the specified sequence
including the arguments of a function. The expressions representing the
arguments of a function are completely evaluated before the function call
proceeds.  This strategy means that the evaluation of an argument happens
irrespective of whether the argument will be actually used inside the function
or not. The evaluation is based on the `static structure of the expression`.
Therefore, the decision can be easily made at compile time.

Stack vs Heap
~~~~~~~~~~~~~

Comparison of strict evaluation and lazy evaluation. Closures vs stack based
evaluation.

..
  Lazy vs Eager Evaluation
  ~~~~~~~~~~~~~~~~~~~~~~~~

  I have come to the conclusion that neither an eager (a/k/a strict, e.g. most languages) nor lazy (a/k/a total, e.g. Haskell) language is vastly superior to the other. They each have tradeoffs, because they are categorical duals.

  Eager is an *inductive* evaluation model, meaning we run the leaves as we as build the nested function tree of runtime possibilities.

  Lazy is a *coinductive* evaluation model, meaning we run the leaves as needed from the nested function tree of runtime possibilities.

  Eager can't instantiate coinductive types, such as bottom (i.e. the entire Universe) or any infinite type, but it can instantiate inductive types such as the natural numbers.

  Lazy can instantiate coinductive types, such as bottom and infinite types, but it can't instantiate inductive types such as the natural numbers.

  See Harper's explanation:

  http://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/#comment-798

  Eager does not have conjunctive products, so we can't do general function composition (e.g. or . map p) without potentially doing evaluation that is not shared (i.e. required) by the conjunctive result.

  Lazy does not have disjunctive sums, so we can't do logic such as:

  ((if ... then 1 else 2),3) == if ... then (1,3) else (2,3)

  For example, if we only evaluate the second element in a lazy language and if ... has an exception, then the left version will not generate the exception, but the right version will.

  I suppose most people think rather inductively.

  '''Space & Latency Indeterminism'''

  Lazy also has latency indeterminism (relative to the imperative world, e.g. IO monad).

  My point is that with eager, debugging the changes in the program's state machine at any function step, will be bounded to the function hierarchy inside the body of the function, so the programmer can correlate changes in the state machine to what the function is expected to do.

  Whereas, with lazy any function may backtrack into functions that were in the argument hierarchy of the current function, and inside functions called an indeterminant time prior. Afaics, lazy debugging should be roughly analogous to debugging random event callbacks, and reverse engineering the state machine in a blackbox event generation module.

  As I understand from Filinksi's thesis, eager and lazy are categorical duals in terms of the induction and coinductive values in the program. Eager doesn't have products (e.g. conjunctive logic, "and") and lazy doesn't have coproducts (e.g. disjunctive, "or"). So this means that lazy imposes imperative control logic incorrectness from the outside-in, because coinductive types are built from observations and their structure (coalgebra) is not known until the finality when the program ends. Whereas, eager's incorrectness is from the inside-out, because inductive types have a a priori known structure (algebra) built from an initiality. Afaics, this explains why debugging eager has a known constructive structure and debugging lazy is analogous to guessing the structure of a blackbox event callback generation module.

  '''Parallel Execution'''

  For a compiler strategy that dynamically subdivided map for parallel execution on multiple cores requires it not be lazy.

References
----------

* https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/informal-semantics.html
* http://www.haskellforall.com/2013/12/equational-reasoning.html
* http://neilmitchell.blogspot.in/2015/02/refactoring-with-equational-reasoning.html
