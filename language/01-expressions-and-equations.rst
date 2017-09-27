.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Expressions and Equations
=========================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+------------------------+----------------------------------------------------+
| Term                   |                                                    |
+------------------------+----------------------------------------------------+
| Expression             |                                                    |
+------------------------+----------------------------------------------------+
| Equation               |                                                    |
+------------------------+----------------------------------------------------+
| Bind                   | assign (bind) a value to a variable                |
+------------------------+----------------------------------------------------+
| Type                   | Denotes rules that a value should conform to       |
|                        | (e.g. Int or String)                               |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+
| Scope                  | Scope of visibility of variable bindings.          |
+------------------------+----------------------------------------------------+

Overview
--------

The most basic building blocks of a Haskell program are expressions and
equations. Everything else is built on top of these concepts. In a later
chapter we will introduce functions as abstractions of expressions. We can say
that everything in Haskell is an expression or an equation which is a named
expression.

Expressions
-----------

Expressions are composed with `terms` and `function` or `operator` applications
on terms.  Operators and functions are essentially the same, except for a
syntactic difference. Simply, an expression has just functions and terms as
the arguments of functions.

Terms
~~~~~

+-----------------------------------------------------------------------------+
| Terms                                                                       |
+=============================================================================+
| A term is a basic building block of an expression.                          |
+--------------------------------+--------------------------------------------+
| Literals                       | Identifiers (LHS of an equation)           |
+------------+---------+---------+----------------+---------------------------+
| Characters | Strings | Numbers | Definitions    | Function names            |
+------------+---------+---------+----------------+---------------------------+
| 'a'        | "Hello" | 10.5    | x              | putStrLn                  |
+------------+---------+---------+----------------+---------------------------+
| Functions names are terms when we use functions as arguments to other       |
| functions or operators e.g. `f` and `g` in `f . g`.                         |
+-----------------------------------------------------------------------------+

Functions
~~~~~~~~~

+-----------------------------------------------------------------------------+
| Function applications                                                       |
+===========+===========+=====================================================+
| print 'a' | even 10   | subtract 0.5 10.5                                   |
+-----------+-----------+-----------------------------------------------------+
| Function names always start with a smallcase alphabetic character and       |
| cannot contain any other special characters except ``_`` or ``'``.          |
+-----------------------------------------------------------------------------+

Operators
~~~~~~~~~

+-----------------------------------------------------------------------------+
| Operators                                                                   |
+=============================================================================+
| A `binary operator` is an `infix function` as the function name is placed   |
| in the middle of its two arguments.                                         |
+---------+---------+---------------------------------------------------------+
| 5 + 4   | 5 - 4   | 2^3                                                     |
+---------+---------+---------------------------------------------------------+
| It becomes obvious that they are really functions if we use them in prefix  |
| notation:                                                                   |
+---------+---------+---------------------------------------------------------+
| (+) 5 4 | (-) 5 4 | (^) 2 3                                                 |
+---------+---------+---------------------------------------------------------+

Composing Expressions
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Expression composition rules                                                |
+=============================================================================+
| A single term is the shortest expression.                                   |
+-----------------------------------------------------------------------------+
| A function or operator application is an expression.                        |
+-----------------------------------------------------------------------------+
| The arguments of a function or operator could be a single literal or        |
| identifier term or an expression (we can also call it an anonymous term!).  |
| If the argument is an expression it must be enclosed in parenthesis for     |
| syntactic disambiguation.                                                   |
+-----------------------------------------------------------------------------+
| These rules can be applied recursively to create compound expressions of    |
| arbitrary complexity.                                                       |
+-------+-----------+------------------------+--------------------------------+
| ``3`` | ``2 + 3`` | ``subtract (2 + 3) 6`` | ``putStrLn (subtract (2 + 3)   |
|       |           |                        | ((5 + 4) + (5 - 4)))``         |
+-------+-----------+------------------------+--------------------------------+

See appendix for operator precedence and associativity.

Evaluating Expressions
~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Evaluating Expressions                                                      |
+=============================================================================+
| Reducing or computing an expression to its final result is called           |
| `reduction` or `evaluation` of the expression.                              |
+-----------------------------------------------------------------------------+
| Expressions are evaluated using the lazy evaluation semantics.              |
+-----------------------------------------------------------------------------+

Equations: Named Expressions
----------------------------

+-----------------------------------------------------------------------------+
| An equation or definition gives a name to an expression:                    |
+-----------+-----------------------------------------------------------------+
| ``<identifier> = <expression>``                                             |
+-----------------------------------------------------------------------------+
| ``v = 10 + 32``                                                             |
+-----------------------------------------------------------------------------+
| Identifier `v` becomes a new term that can be used in another expression.   |
+-----------------------------------------------------------------------------+
| An equation is also called a `binding` as it binds the expression on the RHS|
| to the identifier on the LHS. The identifier `v` (in general, identifiers   |
| on the LHS of an equation) is said to be in a binding position.             |
+-----------------------------------------------------------------------------+
| Identifiers must start with a lower case letter or ``_``.                   |
+-----------------------------------------------------------------------------+

When evaluating an expression, every `reference` to a name (an identifier) in
an expression is `resolved` and  `bound` to some definition or to a name in a
binding position.

Definitions allow you to:

* break bigger expressions into smaller ones
* create reusable expressions

Top level Definitions
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Definitions which are not nested inside any other definition are called     |
| `top level definitions`.                                                    |
+-----------------------------------------------------------------------------+
| Top level definitions have a global scope which means the identifiers bound |
| by these equations are visible to all other equations and their nested      |
| local scopes in the file.                                                   |
+-----------------------------------------------------------------------------+
| ``k = 10``                                                                  |
+-----------------------------------------------------------------------------+
| ``v = k * 2^10``                                                            |
+-----------------------------------------------------------------------------+

Nested Local Definitions
~~~~~~~~~~~~~~~~~~~~~~~~

Definitions may be nested within other definitions.  A definition which is not
nested in any other definition is a `top level declaration`. Each nest level
creates a `scope`. In a given module, at any given scope, we cannot have
multiple definitions with the same name. However, the same name can be defined
at different scopes. When two scopes in hierarchy define the same name, we
`resolve` the name to innermost scope.  The other definitions of the name are
said to be `shadowed` by the chosen definition. The definition to which the
name reference is bound is said to `capture` the reference.

+-----------------------------------------------------------------------------+
| A `let` or `where` clause defines a local scope. Variables introduced in a  |
| local scope are not visible in parent or sibling equation scopes.           |
+-----------------------------------------------------------------------------+
| A binding in local scope shadows a binding of the same name from the parent |
| scopes.                                                                     |
+-----------------------------------------------------------------------------+
| Multiple equations can be defined in a single `let` or `where` clause just  |
| like at the top level.                                                      |
+-----------------------------------------------------------------------------+

Expression-local Definitions (let)
..................................

+-----------------------------------------------------------------------------+
| A `let` clause is an expression with one or more local definitions.         |
+-----------------------------------------------------------------------------+
| Since `let` is an expression it can be used wherever an expression can be   |
| used.                                                                       |
+-----------------------------------------------------------------------------+
| Bindings introduced by let are visible only within the let expression.      |
+-----------------------------------------------------------------------------+
| ``10 + let x = 5 in x * x + 2^3``                                           |
+-----------------------------------------------------------------------------+
| ``let x = 1 in let y = 2 in let z = 3 in x + y + z``                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let x = 1                                                                 |
|       y = 2                                                                 |
|       z = 3                                                                 |
|   in x + y + z                                                              |
+-----------------------------------------------------------------------------+

Equation-local Definitions (where)
..................................

+-----------------------------------------------------------------------------+
| A `where` clause defines one or more equations within the local scope       |
| of another equation.                                                        |
+-----------------------------------------------------------------------------+
| A `where` clause is not an expression in itself therefore unlike `let` it   |
| cannot be embedded arbitrarily inside an expression. It is always at the end|
| of an equation definition.                                                  |
+-----------------------------------------------------------------------------+
| Bindings introduced by where are visible only in the local scope of the     |
| equation it is defined in.                                                  |
+-------------------------+---------------------------------------------------+
| ::                      | ::                                                |
|                         |                                                   |
|  n = x + y + z          |  n = x                                            |
|    where x = 1          |     where x = y + 1                               |
|          y = 2          |              where y = z + 2                      |
|          z = 3          |                       where z = 3                 |
+-------------------------+---------------------------------------------------+

Equation Indentation Rule
~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| When you are writing a multiline equation or multiple equations whether in  |
| GHCi or in a file, you need to ensure that each line is `properly indented`.|
+-----------------------------------------------------------------------------+
| All equations at a given scope (`top level`, `let` or `where`) must start   |
| in the same column.                                                         |
| An equation can continue on the next line in an arbitrary column            |
| as long as it is indented at least one column beyond the start column of    |
| the first line of the equation.                                             |
+-----------------------------------------------------------------------------+
| A `do` expression block has a few more rules described later.               |
+-----------------------------------------------------------------------------+

Recursive Equations
-------------------

Equations provide a way to implement recursion, if there are no equations there
won't be recursion.  An equation can be defined recursively by referring to the
value being defined within the body of the definition.  Any recursive
definition can be reduced to the following normalized version::

  x = f x -- implies f :: a -> a

We can see `x` unfold clearly by repeatedly substituting the term `x` in the
expression for its own definition::

  f x
  f (f x)           -- after substituting x by (f x)
  f (f (f x))       -- after substituting x by (f x)
  ...

You can see that this looks like iteration i.e. applying a function repeatedly
on the previous result. Recursion and iteration are in fact duals of each
other.  Such a recursive non-function definition never terminates if `f` is
strict in its argument.  If `f` discards `x` then the definition just reduces
to a trival non-recursive one.  For example::

    x = f x where f = const 10 -- x = 10

We will see later that non-terminating recursive expression equations can also
be useful.  However, recursion is used most often with functions which are in
fact abstracted expression equations.  It is a very powerful and commonly used
abstraction tool for functions.

Structure of a Haskell Program
------------------------------

A Haskell program is nothing but an equation with a special name called `main`:

+-----------------------------------------------------------------------------+
| ``main = putStrLn "Hello world"``                                           |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Haskell Program: Essentially a set of equations defining functions or data  |
+=============================================================================+
| The program is specified as the equation for the ``main`` function. Parts   |
| of the main equation can be specified using more equations.                 |
+------------+----------------------------------------------------------------+
| Main       | main = putStrLn "hello world!"                                 |
| Equation   |                                                                |
+------------+--------------+-------------------------------------------------+
| Function   | Top level    | f = ``expression``     -- no arguments          |
| Equations  |              +-------------------------------------------------+
|            |              | f a b = ``expression`` -- two arguments         |
|            +--------------+-------------------------------------------------+
|            | let clause   | let f a b c = ``expression``                    |
|            +--------------+-------------------------------------------------+
|            | where clause | where f a b c = ``expression``                  |
+------------+--------------+-------------------------------------------------+
| Data constructors are specified by a data equation                          |
+------------+--------------+-------------------------------------------------+
| Data       | Top level    | data Color = Red | Green | Blue                 |
| Equations  |              |                                                 |
+------------+--------------+-------------------------------------------------+

Mathematical substitution of terms in the equations.

Appendix
--------

Literals
~~~~~~~~

+----------+--------------------------------+---------------------------------+
| Type     | Examples                       | Notes                           |
+==========+==========+========+============+=================================+
| Char     | 'a'      | 'b'    | 'c'        | Unicode character literals      |
+----------+----------+--------+------------+---------------------------------+
| Int      | -1       | 0      | 1          | Signed, Min: 2^63, Max: 2^63 - 1|
+----------+----------+--------+------------+---------------------------------+
| Word     | 0        | 1      | 2          | Unsigned, Min: 0, Max 2^64 - 1  |
+----------+----------+--------+------------+---------------------------------+
| Float    | -5.3     | 0.33333334          | Floating point                  |
+----------+----------+---------------------+---------------------------------+
| Double   | -5.3     | 0.3333333333333333  | Double precision floating point |
+----------+----------+---------------------+---------------------------------+

* TBD string literals
* escape codes
* other ways to write literals e.g. 1.0e7

Arithmetic Operations (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Defined in base
* TODO: point to prelude itself
* TODO: make the expressions executable in ghci

+-----------+----------------+------------------------------------------------+
| Operation | Example        | Description                                    |
+===========+================+================================================+
| \+        | 3 + 2          | Addition                                       |
+-----------+----------------+------------------------------------------------+
| \-        | 3 - 2          | Subtraction                                    |
+-----------+----------------+------------------------------------------------+
| \*        | 3 * 2          | Multiplication                                 |
+-----------+----------------+------------------------------------------------+
| /         | 3 / 2          | Fractional division                            |
+-----------+----------------+------------------------------------------------+
| ^         | 3 ^ 2          | Positive integral power                        |
+-----------+----------------+------------------------------------------------+
| ^^        | 3 ^^ 2         | Integral power                                 |
+-----------+----------------+------------------------------------------------+
| \**       | 3 \** 2.2      | Fractional power                               |
+-----------+----------------+------------------------------------------------+
| div       | 3 \`div\` (-2) | Integral division truncated towards negative   |
|           |                | infinity                                       |
+-----------+----------------+------------------------------------------------+
| mod       | 3 \`mod\` (-2) | modulus of `div`                               |
+-----------+----------------+------------------------------------------------+
| quot      | 3 \`div\` (-2) | Integral division quotient truncated towards   |
|           |                | zero                                           |
+-----------+----------------+------------------------------------------------+
| rem       | 3 \`div\` (-2) | remainder of `quot`                            |
+-----------+----------------+------------------------------------------------+

Function Application
~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `whitespace` or `juxtaposition` is a function application operator. It has  |
| the highest precedence and is left associative (language built-in)          |
+-----------------------------------------------------------------------------+
| f x                                                                         |
+---------+-------------------------------------------------------------------+
| f x y   | (f x) y                                                           |
+---------+-------------------------------------------------------------------+
| f x y z | ((f x) y) z                                                       |
+---------+-------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Think evaluating everything after a `$` before applying it to the function  |
| preceding it (defined in Prelude).                                          |
+-------------+---------------------------------------------------------------+
| f $ x       | f x                                                           |
+-------------+---------------------------------------------------------------+
| f $ g x     | f (g x)                                                       |
+-------------+---------------------------------------------------------------+
| f $ g $ h x | f (g (h x))                                                   |
+-------------+---------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| ``&`` is reverse function application i.e. argument is written before the   |
| function (defined in Prelude).                                              |
+-----------+-----------------------------------------------------------------+
| x & f     | f x                                                             |
+-----------+-----------------------------------------------------------------+
| x & g & f | f (g x)                                                         |
+-----------+-----------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| swap the arguments before applying (defined in Prelude)                     |
+--------------+--------------------------------------------------------------+
| flip f $ x y | f y x                                                        |
+--------------+--------------------------------------------------------------+

Applying a value to a function (continuations)::

  > map ($ 2) [(2*), (4*), (8*)]

Function Composition (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| * ``.`` is composition, lower precedence than function application and      |
|   higher precedence than ``$``.                                             |
| * Note ``(f . g . h)`` applies ``h`` to the argument and then feeds the     |
|   result to ``g`` which feeds the result to ``f``.                          |
+-------------------+---------------------------------------------------------+
| (f . g) x         | f (g x)                                                 |
+-------------------+---------------------------------------------------------+
| f . g $ x         | (f . g) x                                               |
+-------------------+---------------------------------------------------------+
| (f . g . h) x     | f $ g $ h x                                             |
+-------------------+---------------------------------------------------------+
| f . g x           | f . (g x)                                               |
+-------------------+---------------------------------------------------------+

Operators as Functions and Vice Versa (built-in)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------+--------------------------+
| Prefix (function) | Infix (operator)         |
+===================+==========================+
| ``div 4 3``       | ``4 `div` 3``            |
+-------------------+--------------------------+
| ``(+) 4 3``       | ``4 + 3``                |
+-------------------+--------------------------+

+---------------------------------------------+
| Operator Sections                           |
+=============+===============================+
| ``(5 /) x`` | ``5 / x``                     |
+-------------+-------------------------------+
| ``(/ 5) x`` | ``x / 5``                     |
+-------------+-------------------------------+
| ``(5 -) x`` | ``5 - x``                     |
+-------------+-------------------------------+
| Special case: prefix ``-`` is always unary  |
+-------------+-------------------------------+
| ``(- 5)``   | ``-5``                        |
+-------------+-------------------------------+

Operator Fixity
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Operators are just ordinary functions with a default infix syntax.          |
| The only additional property of an operator is its fixity.                  |
| TODO: What makes a valid operator identifier?                               |
+-----------------------------------------------------------------------------+

+---------------+-------------------------------------------------------------+
| Precedence    | Higher precedence operator is evaluated before lower.       |
+---------------+-------------------------------------------------------------+
| Associativity | How operators of the same precedence are grouped in the     |
|               | absence of parentheses.                                     |
+---------------+-------------------------------------------------------------+
| Fixity        | Precedence and associativity together is called fixity      |
+---------------+--------------+--------------+-------------------------------+
| Associative   | (1 + 2) + 3  | 1 + 2 + 3    | 1 + (2 + 3)                   |
+---------------+--------------+--------------+-------------------------------+
| Right         |              | 1 : 2 : []   | 1 : (2 : [])                  |
| Associative   |              |              |                               |
+---------------+--------------+--------------+-------------------------------+
| Left          | ((f x) y) z  | f x y z      |                               |
| Associative   |              |              |                               |
+---------------+--------------+--------------+-------------------------------+

Defining Fixity (Precedence and Associativity)
..............................................

+-------------------+---------------------------------------------------------+
| Default fixity    | Left associative, precedence 9                          |
+-------------------+---------------------------------------------------------+
| Associative       | ``infix <precedence> <op>``                             |
+-------------------+---------------------------------------------------------+
| Left associative  | ``infixl <precedence> <op>``                            |
+-------------------+---------------------------------------------------------+
| Right associative | ``infixr <precedence> <op>``                            |
+-------------------+---------------------------------------------------------+
| Precedence is an integer ranging from 0-9.                                  |
+-----------------------------------------------------------------------------+
| Numerically higher precedence operators are evaluated before lower.         |
+-----------------------------------------------------------------------------+
| Operators at the same precedence cannot be used in a single                 |
| expression without using explicit parenthesis.                              |
+-----------------------------------------------------------------------------+
| There are only two built-in operators i.e. a record creation or update      |
| (``{}``) and function application (whitespace or juxtaposition).            |
+-----------------------------------------------------------------------------+

Fixity of common operators
..........................

+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Groups              | Prec| Op         | Description                              | Left Associative    | Associativity | Right Associative   |
|                     |     |            |                                          |                     | Reason        |                     |
+=====================+=====+============+==========================================+=====================+===============+=====================+
| Functionish (       |     | {}         | Record application (built-in)            | ({...} {...}) {...} |               |                     |
| application, index) +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     |            | Function application (built-in)          | (f x) y             |               |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 9   | .          | Function composition                     |                     | Reduction     | f . (g . h)         |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | !!         | List index                               | (a !! 2) !! 3       |               |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | !          | Map, Array index                         | (a ! 2) ! 3         |               |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ``\\``     | Map subtract                             | ``(a \\ b) \\ c``   | ?             |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | //         | Array append                             | (a // b) // c       | ?             |                     |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Arithmetic (        | 7   | / *        | Multiplication and division              | (1 / 2) / 2         | Rounding      |                     |
| Numeric, list)      +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 6   | \+ -       | Addition and subtraction                 | (1 + 2) + 2         | Overflow      |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 5   | :          | List construction                        |                     |               | 1 : (2 : [])        |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ++         | List append                              |                     | Reduction     | a ++ (b ++ c)       |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ``\\``     | List subtract                            |                                                           |
+---------------------+-----+------------+------------------------------------------+-----------------------------------------------------------+
| Comparisons &       | 4   | == /=      | Comparisons and predicates               |                                                           |
| Boolean             |     | < <= > >=  |                                          |                                                           |
|                     |     | elem       |                                          |                                                           |
|                     |     | notElem    |                                          |                                                           |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 3   | &&         | boolean `and`                            |                     | Reduction     | a && (b && c)       |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 2   | ||         | boolean `or`                             |                     | Reduction     | a || (b || c)       |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Sequencing &        | 1   | >> >>=     | Sequencing                               | (a >> b) >> c       |               |                     |
| Application         |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | &          | reverse function application             | (x & f) & g         |               |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 0   | $          | function application                     |                     |               | f $ (g $ h x)       |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| $ is just opposite of normal function application (juxtaposition or whitespace) i.e. lowest precedence and right associative.                 |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
| Note that only ``:`` and ``$`` are right associative due to inherent semantics, the rest are right associative                                |
| only to force the reduction order of the expression for performance reasons or to force evaluation semantics.                                 |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
| Note also that all left associative operations are left associative because of inherent semantics.                                            |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
| For any other operators not in this table use hoogle to see the fixity in documentation or code.                                              |
+-----------------------------------------------------------------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Some Precedence Examples                                                    |
+==================================+==========================================+
| show R {x = 1, y = 1}            | show (R {x = 1, y = 1})                  |
+----------------------------------+------------------------------------------+
| f . g x                          | f . (g x)                                |
+----------------------------------+------------------------------------------+
| 1 * 2 + 3 + 4 / 5                | (1 * 2) + 3 + (4 / 5)                    |
+----------------------------------+------------------------------------------+
| 1 + 2 : 3 : []                   | (1 + 2) : 3 : []                         |
+----------------------------------+------------------------------------------+
| 1 == 1 && 2 > 1                  | (1 == 1) && (2 > 1)                      |
+----------------------------------+------------------------------------------+
| False && True || True            | (False && True) || True                  |
+----------------------------------+------------------------------------------+
| ``"a" ++ "b" \\ "a"``            | Cannot mix different operators with      |
|                                  | same precedence                          |
+----------------------------------+------------------------------------------+

Binding Position
~~~~~~~~~~~~~~~~

In the following examples `x` is said to be in `binding position` (note this
includes concepts not yet introduced):

+----------------------------+
| x = ...                    |
+----------------------------+
| f x = ...                  |
+----------------------------+
| f (C x) = ...              |
+----------------------------+
| let x = ... in ...         |
+----------------------------+
| let (C x) = ... in ...     |
+----------------------------+
| where x = ...              |
+----------------------------+
| where (C x) = ...          |
+----------------------------+

Scopes
------

TBD


References
----------

* https://en.wikipedia.org/wiki/Scope_(computer_science)
