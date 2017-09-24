.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Expressions & Functions
=======================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+------------------------+----------------------------------------------------+
| Functional language    | Functions are data, just like any other values,    |
|                        | we can pass them as arguments or return them from  |
|                        | functions, compose them together to create a       |
|                        | composite functions.                               |
+------------------------+----------------------------------------------------+
| Abstraction            | Opposites: Reduction, Concretization               |
+------------------------+----------------------------------------------------+
| Abstract, Polymorphic  | Opposites: Concrete, Monomorphic                   |
+------------------------+----------------------------------------------------+
| Instantiation          | Creating a concrete instance of an abstract thing  |
+------------------------+----------------------------------------------------+
| Term                   |                                                    |
+------------------------+----------------------------------------------------+
| Parameters             | The variables denoting arguments of a function     |
+------------------------+----------------------------------------------------+
| Arguments              | Actual parameter values supplied in a function call|
+------------------------+----------------------------------------------------+
| Arity                  | The number of parameters of a function             |
+------------------------+----------------------------------------------------+
| Function application   | Applying a function to its argument(s)             |
+------------------------+----------------------------------------------------+
| Application            | Function application                               |
+------------------------+----------------------------------------------------+
| Partial application    | Function application which supplies less           |
|                        | arguments than the parameters of a function        |
+------------------------+----------------------------------------------------+
| Currying               | In a multi-argument function consuming one         |
|                        | argument at a time and returning another           |
|                        | function which consumes the rest of the arguments. |
+------------------------+----------------------------------------------------+
| Bind                   | assign (bind) a value to a variable                |
+------------------------+----------------------------------------------------+
| Type                   | Denotes rules that a value should conform to       |
|                        | (e.g. Int or String)                               |
+------------------------+----------------------------------------------------+
| Concrete               | Represents a value without any unknowns            |
|                        | (not abstract)                                     |
+------------------------+----------------------------------------------------+
| Monomorphic            | Has only one possible concrete representation      |
+------------------------+----------------------------------------------------+
| Polymorphic            | Has multiple concrete representations (abstract,   |
|                        | not concrete)                                      |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+

Abstraction
-----------

When we have multiple `concrete` objects that have a common structure but
differ only slightly in some parts then we can create an abstract form
representing all the objects by retaining the common parts and replacing the
varying parts with variable `parameters`.  This common form is called an
`abstraction`.  Abstraction essentially separates the common part from variable
parts allowing us to reuse the common part instead of duplicating it.  We can
also call an abstract form a `polymorphic form` since it is a representation of
many different concrete forms.

The variable parameters of an abstract form can later be replaced with specific
values to recover a particular concrete form. By using different values of the
parameters we can recover all possible concrete forms represented by an
abstract form.  Therefore, `abstraction` is a powerful tool to `reuse` code and
remove duplication or redundancy, making code concise.

TBD: Add a picture of colored balls and then abstract it by separating shape
and colors.

There are different levels of abstractions. An abstract form can be further
abstracted creating higher level abstractions. The fundamental abstraction in
Haskell is a `function`. A function abstracts many different forms of an
expression and therefore can also be imagined as a `polymorphic expression`. A
function that can work upon many different data types is an abstraction of many
forms of a function also known as a `polymorphic function`.

The two fundamental aspects of Haskell that bestow power upon the programmer
are the powerful and principled `abstraction` and `composition` facilities.
Principled here means that higher level abstractions are built using reusable
lower level abstractions and always conform to certain rules so that you can be
certain that everything works as expected.

Terms and Expressions
---------------------

An expression is composed of `terms`. The terms in an expression could be
literals e.g. `42`, `'a'`, `"Hello"` or identifiers e.g. `x`.  Terms can also
refer to functions when we are using functions as values e.g. `f` and `g` in `f
. g`.  Terms can be combined to form an expression using an operator
application e.g. `30 + 12`, `7 * 6` or function application e.g. `even 42`.  By
combining terms with functions or operators we can create arbitrarily complex
expressions. An expression can be `evaluated` or `reduced` to a terminal value.

An `equation` or `definition` gives a name to an expression. The LHS of the
following equation i.e. `v` is also called an `identifier` and we say that we
are `binding` the expression to the identifier:

+-----------------------------------------------------------------------------+
| ``v = 10 + 32``                                                             |
+-----------------------------------------------------------------------------+

A Haskell program is nothing but an expression called `main`:

+-----------------------------------------------------------------------------+
| ``main = putStrLn "Hello world"``                                           |
+-----------------------------------------------------------------------------+

Functions: Abstraction of Expressions
-------------------------------------

Multiple similar expressions can be `abstracted` and represented by a common
expression with variable parameters. Such an abstract form of an expression is
called a `function`. When needed the appropriate expression can be recovered by
replacing the variable parameters in the abstract expression by their actual
values via a process reciprocal to `abstraction` called `reduction`.

::

  Concrete   |-------> abstraction    ------>| abstract value
  expression |<------- reduction      <------| aka function
                 aka function application

Abstraction: Defining a Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Writing a haskell program is in fact a process of abstraction that the
programmer goes through.  The functions defined in a program are a result of
that abstraction.  This abstraction process is also called `beta abstraction`
in `lambda calculus` terminology.

+-----------------------------------------------------------------------------+
| A concrete expression has no unknown parameters in it.                      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  c = 10 + 20                                                                |
+-----------------------------------------------------------------------------+

For illustrations we will represent a concrete expression with a complete
rectangle:

::

     +----------+
     |          |
  v0 |          |
     |          |
     |    10    |
     +----------+

+-----------------------------------------------------------------------------+
| Abstracting a concrete expression creates an `abstract expression`,         |
| `parameterized expression` or simply a `function`. A function has one or    |
| variable parameters that can be replaced by actual values later.            |
+-------------------------------+---------------------------------------------+
| concrete value                | ``c0       = 10 + 10 + 10``                 |
+-------------------------------+---------------------------------------------+
| abstract value of arity 1     | ``f1 a     = a  + 10 + 10``                 |
| (one parameter)               |                                             |
+-------------------------------+---------------------------------------------+
| abstract value of arity 2     | ``f2 a b   = a  + b  + 10``                 |
| (two parameters)              |                                             |
+-------------------------------+---------------------------------------------+
| abstract value of arity 3     | ``f3 a b c = a  + b  + c``                  |
| (three parameters)            |                                             |
+-------------------------------+---------------------------------------------+
| `Arity` is the number of parameters in an abstract expressions. It is a     |
| measure of abstraction.                                                     |
+-----------------------------------------------------------------------------+
| The abstracted expression `a + b + c` can be `reused` in place of many      |
| concrete expressions by supplying appropriate values of parameters `a`, `b` |
| and `c`.                                                                    |
+-----------------------------------------------------------------------------+
| Informally an abstract value can also be thought of as an `incomplete value`|
| which becomes complete or concrete when the real values of abstract         |
| variables are applied to it.                                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Function definition                                                         |
+------+------------+---+-----------------------------------------------------+
| Name | Parameters | = | Body                                                |
+------+------------+---+-----------------------------------------------------+
| f3   | a b c      | = | <expression>                                        |
+------+------------+---+-----------------------------------------------------+
| `a`, `b` and `c` are variable `parameters` or `free variables` in the       |
| expression defining the function.                                           |
+-----------------------------------------------------------------------------+
| The Arity of `f3` is three.                                                 |
+-----------------------------------------------------------------------------+
| The abstract expression can be instantiated into a concrete expression      |
| instance by a `function application` (or function call). A function         |
| application would supply the values of parameters as `arguments`.           |
+-----------------------------------------------------------------------------+

For illustrations we represent a function with an incomplete rectangle having a
triangular cut for each variable parameter. The triangle can be imagined as
being filled, completing the value, when an argument is applied:

::

     +-  -  -  -+
     | \/ \/ \/ |
  f  | a  b  c  |
     |          |
     | Value    |
     +----------+
       Arity 3

The input positions of a function are also called, holes or negative
positions. The output position of a function is also called a positive
position.

Reduction: Function Application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Reduction is a process which is opposite of abstraction. A `function        |
| application` concretizes or reduces the abstract expression represented by  |
| a function by `combining` the function with concrete values corresponding   |
| to the abstracted parameters.                                               |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Reduction                                                                   |
+=============================================================================+
| A function application reduces the arity of the function just as an         |
| abstraction increased the arity of the abstract value.                      |
+----------------------+------------------+-----------------------------------+
| Original function    | f3               | ``f3 a b c = a  + b  + c``        |
| (Arity 3)            |                  |                                   |
+----------------------+------------------+-----------------------------------+
| Apply one argument   | f2 = f3 10       | ``f2 b c = 10  + b  + c``         |
| (Arity 2)            |                  |                                   |
+----------------------+------------------+-----------------------------------+
| Apply one argument   | f1 = f2 10       | ``f1 c = 10  + 10  + c``          |
| (Arity 1)            |                  |                                   |
+----------------------+------------------+-----------------------------------+
| concrete expression  | c0 = f1 10       | ``10  + 10  + 10``                |
+----------------------+------------------+-----------------------------------+
| Each application results in an exprssion (function) of reduced arity        |
| finally yielding a concrete expression.                                     |
+-----------------------------------------------------------------------------+
| We can also apply multiple arguments at a time:                             |
+----------------------+------------------+-----------------------------------+
| function of arity 1  | f1 = f3 10 10    | ``f1 c = 10  + 10  + c``          |
+----------------------+------------------+-----------------------------------+
| concrete expression  | c0 = f3 10 10 10 | ``c0 = 10  + 10  + 10``           |
+----------------------+------------------+-----------------------------------+
| If the number of arguments are less than the arity of the funciton then it  |
| is called a `partial application` or `currying`. `f1` and `f2` above are    |
| results of partial application and also called `curried` functions.         |
+-----------------------------------------------------------------------------+
| This process of applying a value to reduce the abstraction is also called   |
| `beta reduction` in lambda calculus. Reduction happens during the process   |
| of evaluation of a program.                                                 |
+-----------------------------------------------------------------------------+

Function Application
--------------------

+-----------------------------------------------------------------------------+
| Function application                                                        |
+========+===+======+=========================================================+
| Output | = | Name | arguments                                               |
+--------+---+------+---------------------------------------------------------+
| r      | = | f3   | v1 v2 v3                                                |
+--------+---+------+---------------------------------------------------------+
| Parameters `a`, `b` and `c` in the function definition get `bound` to the   |
| input arguments `v1`, `v2` and `v3` respectively when the function is       |
| applied.                                                                    |
+-----------------------------------------------------------------------------+

::

    10                  10                  10
    \/                  \/                  \/
  +-  -  -  -+     +----  -  -+     +-------  -+     +----------+
  | \/ \/ \/ |     |    \/ \/ |     |       \/ |     |          |
  | a  b  c  |     |    b  c  |     |       c  |     |          |
  |          |     |          |     |          |     |          |
  | Value    |     | Value    |     | Value    |     | Value    |
  +----------+     +----------+     +----------+     +----------+
   Arity 3           Arity 2          Arity 1          Concrete

+-----------------------------------------------------------------------------+
| More details on function application:                                       |
+=============================================================================+
| We can think of `juxtaposition` of a function and its argument (or the      |
| `whitespace` between them) in `f a` as a reduction or function application  |
| operator.                                                                   |
+-----------------------------------------------------------------------------+
| Whitespace as an operator may be clearer if we imagine some other operator  |
| symbol in place of whitespace e.g. ``f $ a $ b $ c``                        |
+-----------------------------------------------------------------------------+
| Everything is a value! `f a` combines an abstract value `f` with the value  |
| `a` to produce a more concrete value.                                       |
+-----------------------------------------------------------------------------+
| Function application is an asymmetric operation because `f` and `a`         |
| have different roles, which means the operation is not commutative i.e.     |
| `f a` is not the same as `a f`                                              |
+-----------------------------------------------------------------------------+
| This operation is left associative i.e. ``f a b c <=> ((f a) b) c``         |
+-----------------------------------------------------------------------------+

Types of Terms and Expressions
------------------------------

A common programming mistake in `untyped` or weakly typed languages is using a
wrong value i.e. use an `orange` in a computation where we were supposed to use
an `apple`. How do we avoid such mistakes and ensure the correctness of a
program?

Haskell expressions are made of functions applied to terms. Every term in an
expression has a unique `type` associated with it.  A type is a label that
determines the legal values that the data can assume.  A function application
in an expression is the `only way` to combine values and produce new values.
The typechecker knows what type of arguments a function expects and if the
types applied to it do not match compilation fails. The type of an expression
is inferred from the type resulting from the outermost function application in
the expression. The argument type of the function that consumes this expression
must match the type of the expression. This process goes on until the whole
program satisfies the requirement that all appplications are using correct
types.

A programmer can define new data types e.g. an orange can be assigned an
`Orange` type and an apple can be assigned an `Apple` type. If a function
argument is inferred to accept an `Orange` at one place you cannot pass an
`Apple` to it at another place. This way everything is well-typed and there is
no chance of passing an incorrectly typed value anywhere in the program.
Haskell provides very strong type checking guarantees.

Types can usually be inferred via `type inference` but if there is an ambiguity
the programmer can explicitly specify the type of an expression or function
using `type signatures` (also known as `type annotations`).

Type Signatures
~~~~~~~~~~~~~~~

Let's take an example of an identifier `v` representing a concrete data value::

     Value              Type
  +----------+         +----------+
  |          |         |          |
  |          |   v     |          |
  |          |         |          |
  |   33     |         |   Int    |
  +----------+         +----------+


+-----------------------------------------------------------------------------+
| Types are associated to a value by a `type signature`.                      |
+---------------------------------+-------------------------------------------+
| v :: Int                        | Type Level Program (type signature)       |
+---------------------------------+-------------------------------------------+
| v = 33                          | Term Level Program (value equation)       |
+---------------------------------+-------------------------------------------+
| Identifier `v` represents the value ``33`` of type ``Int``.                 |
| `Term level program` uses an `=` to bind an identifier to a value while the |
| `type level program` uses a `::` to bind an identifier to a type.           |
+-----------------------------------------------------------------------------+

Now, let's take an example of a function::

        Value                    Type

     +-  -  -  -+       +--  ---  ----  ---+
     | \/ \/ \/ |       |  \/   \/    \/   |
  f  | a  b  c  |       | Char  Int  Int   |
     |          |       |                  |
     | Value    |       |       Char       |
     +----------+       +------------------+
       Arity 3

+-----------------------------------------------------------------------------+
| Type signature of a function:                                               |
+---------------------------------+-------------------------------------------+
| f :: Char -> Int -> Int -> Char | Type Level Program                        |
+---------------------------------+-------------------------------------------+
| f a b c = ...                   | Term Level Program                        |
+---------------------------------+-------------------------------------------+
| Every input and the output parameter of a function has a type associated    |
| with it.                                                                    |
+-----------------------------------------------------------------------------+
| ``->`` is an infix `type function` which generates the type for this        |
| data function by using the types of its parameters as well as the return    |
| type as arguments. The argument ``a`` has type ``Char``, ``b`` has type     |
| ``Int``, ``c`` has type ``Int`` and the return type of the function is      |
| ``Char``.                                                                   |
+-----------------------------------------------------------------------------+

Type Checking
~~~~~~~~~~~~~

The onus of assigning unique types to different data items is on the programmer
so that distinct types of values cannot accidentally be used in place of each
other.  The types are analyzed at compile time by the `typechecker`.  It
essentially checks if the types used in the program are consistent and we are
not using one type in place of another. Type checks include:

* `functions`: The type of the function input must match the type of the value
  being fed to the function.

* `Equations`: When two values can be substituted in place of each other then
  they must have the same type.

Term & Type Level Programming
-----------------------------

A Haskell program is an expression consisting of terms and function
applications. The terms or functions used in an expression may be defined by
independent equations.  We will call building this expression and parts of it
as the `term level program`.

Each term and function used in the expression has a type associated with it.
The types are specified via type signatures. We can call these type annotations
collectively as the `type level program`. The type level programming can be as
advanced as the term level programming itself as we will see later.

References
----------

* https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser
