Template Haskell
================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+------------------------+----------------------------------------------------+
| TH                     | Template Haskell                                   |
+------------------------+----------------------------------------------------+
| reification            |                                                    |
+------------------------+----------------------------------------------------+
| splice                 |                                                    |
+------------------------+----------------------------------------------------+
| template               |                                                    |
+------------------------+----------------------------------------------------+
| Quasi-quotation        | Quasiquotation allows programmers to generate      |
|                        | code automatically from code templates; the        |
|                        | “quasi” in quasiquotation refers to the fact that  |
|                        | these code templates can contain holes that are    |
|                        | filled in by the programmer                        |
+------------------------+----------------------------------------------------+
| anti-quotation         | Antiquotation, as the name suggests goes in the    |
|                        | opposite direction: embeds Haskell entities        |
|                        | (e.g. variables) in our DSL.                       |
+------------------------+----------------------------------------------------+
| object-language        |                                                    |
+------------------------+----------------------------------------------------+

What is it?
-----------

Template Haskell provides compile-time meta-programming facilities allowing
programmers to compute some parts of their program at compile time rather than
write them.  Template Haskell can be viewed both as a template system (a la `
C++), as well as a type-safe macro system.

The ability to generate code at compile time allows the programmer to implement
such features as polytypic programs, macro-like expansion, user directed
optimization (such as inlining), and the generation of supporting data
structures and functions from existing data structures and functions.

Template Haskell is strongly typed in the Milner sense: a welltyped
program cannot “go wrong” at run-time.

The Haskell world has Template Haskell (Sheard and Peyton Jones
2002) which similarly allows Haskell programs to construct other
Haskell programs. These “program generating programs” are one
type of metaprogram, programs that manipulate other programs as
data. In both the Lisp and Template Haskell worlds, the language
in which the metaprogram is written, or metalanguage, is identical
to the language in which the manipulated programs are written, the
object language.

What is it useful for?
----------------------

We envision that Template Haskell will be used by programmers to
do many things.

* Conditional compilation is extremely useful for compiling a
  single program for different platforms, or with different debugging
  options, or with a different configuration. A crude
  approach is to use a preprocessor like cpp — indeed several
  compilers for Haskell support this directly — but a mechanism
  that is part of the programming language would work
  much better.

* Program reification enables programs to inspect their own
  structure. For example, generate a function to serialise a data
  structure, based on the data type declaration for that structure.

* Algorithmic program construction allows the programmer to
  construct programs where the algorithm that describes how
  to construct the program is simpler than the program itself.
  Generic functions like map or show are prime examples, as
  are compile-time specialized programs like printf, where
  the code compiled is specialized to compile-time constants.

* Abstractions that transcend the abstraction mechanisms accessible
  in the language. Examples include: introducing
  higher-order operators in a first-order language using
  compile-time macros; or implementing integer indexed functions
  (like zip1, zip2, ... zipn) in a strongly typed language.

* Optimizations may teach the compiler about domain-specific
  optimizations, such as algebraic laws, and in-lining opportunities.

How it works?
-------------

A programmer writes a `template` or `splice` which is Haskell code, and is used
to generate a representation of some Haskell source at compile time. These
templates can be spliced with the host program source at locations chosen by
the programmer. Just like macro pre-processing, in a pre-compile phase the
templates are first compiled and then run in the context of the hosting source
(at the splice point) to generate the desired Haskell source. The generated
Haskell source is then spliced with the host program source and the combined
Haskell program is then compiled.

As a trivial example, the expression ``5 + 1`` can be evaluated and replaced by
``6`` at compile time. Arbitrary Haskell source code can be represented and
produced by template haskell. We can examine and use information about the
hosting source to expand the splice. We can even perform IO to gather
information to generate the source.

So in Template Haskell type checking takes place in stages:

* First type check the body of the splice; in this case it is::

    (printf "Error: %s on line %d") :: Expr.

* Next, compile it, execute it, and splice the result in place of
  the call. In our example, the program now becomes::

    (\ s0 -> \ n1 ->
    "Error: " ++ s0 ++ " on line " ++ show n1)
    "urk" 341

* Now type-check the resulting program, just as if the programmer
  had written that program in the first place.
  Hence, type checking is intimately interleaved with (compile-time)
  execution.

Template Haskell is a compile-time only meta-system. The metalevel
operators (brackets, splices, reification) should not appear in
the code being generated. For example, [| f [| 3 \|] \|] is illegal.

There are other restrictions as well. For example, this definition
is illegal (unless it is inside a quotation)::

  f x = $(zipN x)

Why? Because the “$” says “evaluate at compile time and splice”,
but the value of x is not known until f is called. This is a common
staging error.

To enforce restrictions like these, we break the static-checking part
of the compiling process into three states. Compiling (C) is the state
of normal compilation. Without the meta-operators the compiler
would always be in this state. The compiler enters the state Bracket
(B) when compiling code inside quasi-quotes. The compiler enters
the state Splicing (S) when it encounters an expression escape inside
quasi-quoting brackets. For example, consider::

  f :: Int -> Expr
  f x = [| foo $(zipN x) |]

The definition of f is statically checked in state C, the call to foo is
typed in state B, but the call to zipN is typed in state S.
In addition to the states, we count levels, by starting in state 0, incrementing
when processing under quasi-quotes, and decrementing
when processing inside $ or splice. The levels are used to distinguish
a top-level splice from a splice inside quasi-quotes. For
example::

  g x = $(h [| x*2 |])

The call to h is statically checked in state S at level -1, while the
x*2 is checked in state B at level 0.

Example
-------

Overview
--------

A programming language consists of a concrete syntax and an abstract syntax.
Concrete syntax is what the programmer writes. Abstract syntax is a
representation of the source program as a data structure.  It is represented as
a tree data structure (Abstract Syntax Tree or AST) with each node representing
a specific construct in the concrete syntax. The abstract syntax tree is
represented as an ADT in Haskell.

A template Haskell expression (splice) is first compiled and then run to
evaluate it to Haskell's abstract syntax and the resulting AST entity is
spliced with the AST of the source program at the specified splice point.

To summarize, in Template Haskell there are three “layers” to the
representation of object-programs, in order of increasing convenience
and decreasing power:

* The bottom layer has two parts. First, ordinary algebraic data
  types represent Haskell program fragments (Section 6.2).
  Second, the quotation monad, Q, encapsulates the notion of
  generating fresh names, as well as failure and input/output
  (Section 8).

* A library of syntax-construction functions, such as tup and
  app, lift the corresponding algebraic data type constructors,
  such as Tup and App, to the quotation-monad level, providing
  a convenient way to access the bottom layer (Section 6.3).

* The quasi-quote notation, introduced in Section 2, is most
  convenient but, as we have seen, there are important metaprograms
  that it cannot express. We will revisit the quasiquote
  notation in Section 9, where we show how it is built on
  top of the previous layers.

The programmer can freely mix the three layers, because the latter
two are simply convenient interfaces to the first. We now discuss in
more detail the first two layers of code representation. We leave a
detailed discussion of quasi-quotes to Section 9

Constructing Splices
--------------------

Algebraic Datatype Representation of Haskell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following list represents the constructs needed to represent the Haskell
AST. We can generate them programmatically using TH and then splice them back
in the source program.  These constructs in fact cover everything in any
Haskell program source! Each construct is represented by a Haskell algebraic
data type (ADT).

+-----------+-----------------------------------------------------------------+
| Haskell   | Description                                                     |
| ADT       |                                                                 |
+===========+=================================================================+
| Type      | Represents a type declaration in Haskell source                 |
|           | (e.g. ``x :: Int``)                                             |
+-----------+-----------------------------------------------------------------+
| Dec       | Represents a top level declaration (e.g. ``f x = x * x``)       |
+-----------+-----------------------------------------------------------------+
| Exp       | An expression (e.g. ``x * x``)                                  |
+-----------+-----------------------------------------------------------------+
| Pat       | A pattern                                                       |
+-----------+-----------------------------------------------------------------+

The `Language.Haskell.TH` module from the `template-haskell` package provides
algebraic data types to represent these entities.  TBD - provide hyperlinks.

The particular data types used for Template Haskell are given in
Appendix B. The highlights include algebraic datatypes to represent
expressions (Exp), declarations (Dec), patterns (Pat), and
types (Typ). Additional data types are used to represent other syntactic
elements of Haskell, such as guarded definitions (Body), do
expressions and comprehensions (Statement), and arithmetic sequences
(DotDot). We have used comments freely in Appendix B
to illustrate the algebraic datatypes with concrete syntax examples.

TBD: Typed expression?

TBD: introduce this in the basic syntax chapter. The `Language.Haskell.TH`
module can also act as a guide to the syntax.

The Quotation Monad
~~~~~~~~~~~~~~~~~~~

The `Q` monad allows us to compose a TH splice representing an AST
data type, in a stateful manner, by querying the host source code, source
location, report errors, and even perform IO to get information for creating
the splice. The final output is one of the AST entities that we described in
the previous section.

  runQ :: Quasi m => Q a -> m a

The quotation monad encapsulates meta-programming features such as fresh name
generation, program reification, and error reporting. A monadic library of
syntax operators is built on top of the algebraic datatypes and the quotation
monad. It provides an easy-to-use interface to the meta-programming parts of
the system

Reification involves making the internal representation of T available as a
data structure to compile-time computations.

The reification facilities of the quotation monad allows the programmer (at
compile-time) to query the compiler’s internal data structures, asking
questions such as “What is the line number in the source-file of the current
position?” (useful for error reporting), or “What is the kind of this type
constructor?”

* Fresh name generation
* Reification
* Failure:
  A compile-time meta-program may fail, because the programmer
  made some error
* IO:
  A meta-program may require access to input/output facilities. For
  example, we may want to write::

    splice (genXML "foo.xml")

  to generate a Haskell data type declaration corresponding to the
  XML schema stored in the file "foo.xml", together with some
  boilerplate Haskell functions to work over that data type.

* Printing code
  To display code constructed in the computational framework we
  supply the function runQ :: Q a -> IO a. Thus, if we compile
  and run the program::

    main = do { e <- runQ (sel 1 3) ; putStr (show e) }

Types of Splices
~~~~~~~~~~~~~~~~

The Q monad is used to compose and return a TH splice having one of the
following algebraic data types:

+--------------+--------------------------------------------------------------+
| Splice Type  | Represents an AST construct for                              |
+==============+==============================================================+
| Q Exp        | An expression                                                |
+--------------+--------------------------------------------------------------+
| Q (TExp a)   | A typed expression                                           |
+--------------+--------------------------------------------------------------+
| Q Pat        | A pattern                                                    |
+--------------+--------------------------------------------------------------+
| Q Type       | A type                                                       |
+--------------+--------------------------------------------------------------+
| Q [Dec]      | A list of declarations at top level                          |
+--------------+--------------------------------------------------------------+

These data types (also known as splices) are the interface between template
haskell and the Haskell source where they are spliced in. For example, a splice
of type ``Q Exp`` can be used wherever we can use an expression in the Haskell
source.

Splices are in turn constructed using various AST data constructors described
in the `template-haskell` package.  Note that a splice is merely an ordinary
Haskell algebraic data type, it can be passed around in functions, can be used
as input to generate other splices.

See Where can they occur? for using partial type signatures in quotations.

Applying Splices
----------------

An AST data type returned by the Q monad can be expanded or spliced at an
appropriate point in the Haskell source. Splicing acts as if equivalent
Haskell source was written at that point.

The host source context uniquely determines the type of the splice that can be
used at that point. For example ``f = $x`` requires the splice ``x`` to be of
type ``Q Exp`` because it is being used in an expression context.

+-----------------------------------------------------------------------------+
| `-XTemplateHaskell`: Enable Template Haskell’s splice and quotation syntax. |
+-----------------------------------------------------------------------------+
| A symbol (``x``) or expression (``expr``) representing a Haskell AST of     |
| type ``Q Exp``, ``Q Type``, ``Q [Dec]`` or ``Q Pat`` is spliced in the      |
| Haskell source (at compile time) using the TH splicing syntax.              |
+-----------+-----------------------------------------------------------------+
| $x        | Expand an identifier `x` representing a TH splice               |
+-----------+-----------------------------------------------------------------+
| x         | Same as ``$x`` but can be used `only at the top level`          |
+-----------+-----------------------------------------------------------------+
| $(expr)   | Expand an expression `expr` representing a TH splice            |
+-----------+-----------------------------------------------------------------+
| expr      | Same as ``$(expr)`` but can be used `only at the top level`     |
+-----------+-----------------------------------------------------------------+
| A typed expression splice of type ``Q (TExp a)``                            |
+-----------+-----------------------------------------------------------------+
| $$x       | Expand an identifier representing a typed expression splice     |
+-----------+-----------------------------------------------------------------+
| $$(expr)  | Expand an expression representing a typed expression splice     |
+-----------+-----------------------------------------------------------------+
| `x` and `expr` can only use imported symbols, and not symbols defined       |
| elsewhere within the same module.                                           |
+-----------------------------------------------------------------------------+
| Declaration splices are not allowed anywhere except at top level.           |
+-----------------------------------------------------------------------------+
| Haskell can have only declarations at the top level. Therefore, at the top  |
| level, the compiler can distinguish a TH splice use even without the `$`    |
| because it is always an expression.                                         |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| GHC stage restriction                                                       |
+-----------------------------------------------------------------------------+
| A splice expansion cannot use symbols defined in the same module.           |
| The following examples result in an error.                                  |
+--------------------------+------------------+-------------------------------+
| Defining module          | Using module     | Error reason                  |
+--------------------------+------------------+-------------------------------+
|                          | ::               |                               |
|                          |                  |                               |
|                          |  expr = [| x \|] |                               |
|                          |  v = $expr       | expr must be imported         |
+--------------------------+------------------+-------------------------------+
| ::                       | ::               |                               |
|                          |                  |                               |
|                          |  y = 0           |                               |
|  expr x = [| x \|]       |  v = $(expr y)   | y must be imported            |
+--------------------------+------------------+-------------------------------+
| ::                       | ::               |                               |
|                          |                  |                               |
|  y = 0                   |                  |                               |
|  expr x = [| x \|]       |  v = $(expr y)   | ok.                           |
+--------------------------+------------------+-------------------------------+

Library of Syntax Construction Functions
----------------------------------------

Library of Monadic Syntax Operators

In general, we use the following nomenclature:
* A four-character type name (e.g. Expr) is the monadic version
of its three-character algebraic data type (e.g. Exp).
* A lower-cased function (e.g. app) is the monadic version of
its upper-cased data constructor (e.g. App)

Constructors
~~~~~~~~~~~~

+-------+--------+------------------------------------------------------------+
| Type  | Suffix | Constructor examples                                       |
+=======+========+============================================================+
| Exp   | E      | varE, conE, LitE, LamE                                     |
+-------+--------+------------------------------------------------------------+
| Pat   | P      | varP, conP, LitP                                           |
+-------+--------+------------------------------------------------------------+
| Type  | T      | varT, conT, LitT                                           |
+-------+--------+------------------------------------------------------------+
| Dec   | D      | FunD, ValD, DataD                                          |
+-------+--------+------------------------------------------------------------+

Functions
~~~~~~~~~

Conventions: returning type `Q Exp` end with `E` and so on.

Constructing Splices using Syntax Operators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using the Q monad and the TH lib functions::

  f = $(do
    nm1 <- newName "x"
    let nm2 = mkName "x"
    return (LamE [VarP nm1] (LamE [VarP nm2] (VarE nm1)))
   )
  will produce the splice

  f = \x0 -> \x -> x0

Name Resolution & Capture
~~~~~~~~~~~~~~~~~~~~~~~~~

AST constructs can refer to names of functions, data constructors or types etc.
A name is represented in the AST by the ``Name`` data type.  Names can be used
to construct Template Haskell expressions, patterns, declarations etc.

In template Haskell there are two different places for name resolution, (1)
where the splice is defined, (2) where the splice is used. There are different
APIs/syntax for referring to names in these two scopes.

Names can be of two types from capture perspective, (1) names that can be
captured as per regular Haskell scoping and cature semantics, (2) names that
cannot be captured i.e. they are not resolved against other names and are
therefore local to the splice template.

Creating a Name
~~~~~~~~~~~~~~~

+---------------------------------+----------------+--------------------------+
| ``mkName :: String -> Name``    | Capturable     | Pure API                 |
+---------------------------------+----------------+--------------------------+
| ``newName :: String -> Q Name`` | Not capturable | Monadic API              |
+---------------------------------+----------------+--------------------------+

Referring to existing names
~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Referring to names currently in scope (at definition or use site).          |
+--------------------+-------------------------+------------------------------+
| Namespace          | Splice definition scope | Splice use site scope        |
|                    | (``:: Name``)           | (``:: Q (Maybe Name)``)      |
+====================+=========================+==============================+
| value (function    | 'f                      | lookupValue "f"              |
| or constructor)    |                         |                              |
+--------------------+-------------------------+------------------------------+
| type               | ''T                     | lookupValue "T"              |
+--------------------+--------------------------------------------------------+
| A name whose second character is a single quote (sadly) cannot be quoted in |
| this way. For example, if the function is called f'7, an attempt to quote   |
| it as 'f'7 would be parsed as the character literal 'f' followed by the     |
| numeric literal 7.                                                          |
+-----------------------------------------------------------------------------+
| These names can never be captured.                                          |
+-----------------------------------------------------------------------------+
| These names may not be used in bindings                                     |
| (such as ``let x = ...`` or ``x -> ...``)                                   |
+-----------------------------------------------------------------------------+

Dynamic scoping
~~~~~~~~~~~~~~~

Occasionally, the programmer may instead want a dynamic scoping
strategy in generated code. In Template Haskell we can express
dynamic scoping too, like this::

  genSwapDyn x = [| $(var "swap") x |]

Now a splice site $(genSwapDyn (4,5)) will expand to
(swap (4,5)), and this swap will bind to whatever swap is in
scope at the splice site, regardless of what was in scope at the defi-
nition of genSwapDyn. Such behaviour is sometimes useful, but in
Template Haskell it is clearly flagged by the use of a string-quoted
variable name, as in (var "swap"). All un-quoted variables are
lexically scoped.

+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|  x = 1                              |  x = 2           |                    |
|  expr = varE (mkName "x")           |  v = $expr       |  v = 2             |
+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|  x = 1                              |  x = 2           |                    |
|  expr = do                          |  v = $expr       |  v = 2             |
|      Just nm <- lookupValueName "x" |                  |                    |
|      [| $(varE nm) |]               |                  |                    |
+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|  x = 1                              |  x = 2           |                    |
|  expr = [|$(varE 'x)|]              |  v = $expr       |  v = 1             |
+-------------------------------------+------------------+--------------------+

* The lookup is performed in the context of the top-level splice being run. For
  example::

    f = "global"
    g = $( [| let f = "local" in
               $(do
                   Just nm <- lookupValueName "f"
                   varE nm
                ) |] )

* Operators should be queried without any surrounding parentheses, like so::

    lookupValueName "+"

Querying Names
~~~~~~~~~~~~~~

* reify
* reifyModule
* thisModule

Termplates with Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A template can be defined with parmeters which are supplied at the splice   |
| point.                                                                      |
+-----------------------------------+-----------------------------------------+
| Definition site                   | Use site                                |
+-----------------------------------+-----------------------------------------+
| ::                                | ::                                      |
|                                   |                                         |
|  module Bar where                 |  module Foo where                       |
|                                   |                                         |
|  import Language.Haskell.TH       |  import Bar                             |
|                                   |                                         |
|  add1 :: Int -> Q Exp             |  two :: Int                             |
|  add1 x = [| x + 1 \|]            |  two = $(add1 1)                        |
+-----------------------------------+-----------------------------------------+

Template Haskell cannot know what the argument to add1 will be at the
function’s definition site, so a lifting mechanism is used to promote x into a
value of type Q Exp. This functionality is exposed to the user as the Lift
typeclass in the Language.Haskell.TH.Syntax module. If a type has a Lift
instance, then any of its values can be lifted to a Template Haskell
expression::

  class Lift t where
      lift :: t -> Q Exp

In general, if GHC sees an expression within Oxford brackets (e.g., [| foo bar
\|], then GHC looks up each name within the brackets. If a name is global (e.g.,
suppose foo comes from an import or a top-level declaration), then the fully
qualified name is used directly in the quotation. If the name is local (e.g.,
suppose bar is bound locally in the function definition mkFoo bar = [| foo bar
\|]), then GHC uses lift on it (so GHC pretends [| foo bar \|] actually contains
[| foo $(lift bar) \|]). Local names, which are not in scope at splice
locations, are actually evaluated when the quotation is processed.

The template-haskell library provides Lift instances for many common data
types. Furthermore, it is possible to derive Lift instances automatically by
using the -XDeriveLift language extension. See Deriving Lift instances for more
information.

Quasiquotation for Haskell Source
---------------------------------

The compiler provides a built-in quotation syntax using Oxford brackets to
generate splices by just `quoting` Haskell source.  The result of the quoted
expression is a splice data type (e.g. ``Q Exp``) corresponding to the quoted
source. This is a more convenient (high level) way of building splices.

+-----------------------------------------------------------------------------+
| `-XTemplateHaskellQuotes`: Enable only Template Haskell’s quotation syntax. |
+---------------------------------------+-------------------------------------+
| [| <expression> \|]                   | Q Exp                               |
+---------------------------------------+                                     |
| [e| <expression> \|]                  |                                     |
+---------------------------------------+-------------------------------------+
| [|| <typed expression> \||]           | Q (TExp a)                          |
+---------------------------------------+                                     |
| [e|| <typed expression> \||]          |                                     |
+---------------------------------------+-------------------------------------+
| [d| <list of declarations> \|]        | Q [Dec]                             |
+---------------------------------------+-------------------------------------+
| [t| <type signature> \|]              | Q Type                              |
+---------------------------------------+-------------------------------------+
| [p| <pattern> \|]                     | Q Pat                               |
+---------------------------------------+-------------------------------------+
| [varid| <an arbitrary string> \|]     | Quasi quotation                     |
+---------------------------------------+-------------------------------------+

Lexical Scoping
~~~~~~~~~~~~~~~

The quasi-quote notation is a convenient shorthand for representing
Haskell programs, and as such it is lexically scoped. More precisely:
every occurrence of a variable is bound to the value that
is lexically in scope at the occurrence site in the original
source program, before any template expansion.

To summarize, lexical scoping means that the free variables (such
as swap and x) of a top-level quasi-quote (such as the right hand
side of the definition of genSwap) are statically bound to the closure.
They do not need to be in scope at the application site (inside
module Foo in this case); indeed some quite different value of the
same name may be in scope.

The quasi-quote notation is implemented on top of the quotation
monad (Section 6), and we saw there that variables bound inside
quasi-quotes must be renamed to avoid inadvertent capture (the
cross2a example). But that is not all; what about variables bound
outside the quasi-quotes?

+-------------------------------------+------------------+--------------------+
| Definition                          | Use              | Result             |
+-------------------------------------+------------------+--------------------+
|                                     | ::               | ::                 |
|                                     |                  |                    |
|                                     |  v = $([|2|])    |  v = 2             |
+-------------------------------------+------------------+--------------------+
|                                     | ::               | ::                 |
|                                     |                  |                    |
|                                     |  x = 2           |                    |
|                                     |  v = $([|x|])    |  v = 2             |
+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|  x = 1                              |  x = 2           |                    |
|  expr = [|x|]                       |  v = $expr       |  v = 1             |
+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|                                     |  x = 2           |                    |
|  expr = [|x|]                       |  v = $expr       |  v = 2             |
+-------------------------------------+------------------+--------------------+
| ::                                  | ::               | ::                 |
|                                     |                  |                    |
|  x = 1                              |  x = 2           |                    |
|  expr k = [|x + k|]                 |  v = $(expr 5)   |  v = 6             |
+-------------------------------------+------------------+--------------------+

Mixing Quasiquotation and Syntax Operators
------------------------------------------

Values of type ``TExp a`` may be converted to values of type ``Exp`` using the
function ``unType :: TExp a -> Exp``.

Since we have plain Haskell inside the quotes we can even use splices (e.g.
varE and mkName in the example below) inside the quotes.

::

  f = [| pi + $(varE (mkName "pi")) |]
  ...
  g = let pi = 3 in $f

  In this case, g is desugared to

  g = Prelude.pi + 3

Note that quotations are just values of type Q Exp, Q Dec etc.  They can be
used wherever we can use those types. For example we can use quotations inside
the Q monad::

  f = "global"
  g = $( do
           Just nm <- lookupValueName "f"
           [| let f = "local" in $( varE nm ) |]

Quasiquotation for Arbitrary DSLs
---------------------------------

There are many cases when it would be useful to
have an object language that is different from the metalanguage.
The canonical example of a metaprogram is a compiler, which typically
manipulates many different intermediate object languages
before producing a binary. Other potential applications that could
benefit from a more flexible quasiquoting system include peephole
optimizers, partial evaluators, and any source-to-source transformation.
The ability to quasiquote arbitrary object languages means
the programmer can think about and write programs using the concrete
syntax best suited to the domain, be it C, regular expressions,
XML or some other language. Although we find support for
quasiquoting arbitrary languages most compelling in the context of
metaprogramming, quasiquoting is useful any time a complex data
type can be given concrete syntax

References
----------

* https://www.schoolofhaskell.com/user/marcin/template-haskell-101
* https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial
* https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/meta-haskell.pdf
* https://www.schoolofhaskell.com/user/marcin/quasiquotation-101
* http://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf
* https://www.schoolofhaskell.com/user/edwardk/bound
