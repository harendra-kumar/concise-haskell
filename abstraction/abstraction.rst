.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Abstraction: Data & Functions
=============================

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
| Value                  | A generic term for anything that can be accepted as|
|                        | arguments by a function. It could be a function or |
|                        | data in a data level program, or types or type     |
|                        | functions in a type level program.                 |
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
| First order function   | None of the arguments is a function                |
+------------------------+----------------------------------------------------+
| Higher order functions | One or more argument is a function                 |
+------------------------+----------------------------------------------------+
| Data                   | A way to represent values to be communicated across|
|                        | functions in a data level program. Data is         |
|                        | constructed using algebraic data types.            |
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
| Monotype               | A monomorphic type                                 |
+------------------------+----------------------------------------------------+
| Kind                   | Type of types (e.g. a type could be lifted or      |
|                        | unlifted)                                          |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+
| Universal              |                                                    |
| quantification         |                                                    |
+------------------------+----------------------------------------------------+
| Existential            |                                                    |
| quantification         |                                                    |
+------------------------+----------------------------------------------------+


Abstraction
-----------

`Abstraction` is a powerful tool to `reuse` code and remove duplication or
redundancy. When we have multiple `concrete` objects that have a common
structure but differ only slightly in some parts then we can create an abstract
form representing all the objects by retaining the common parts and abstracting
the varying parts, replacing them with variable parameters.  This common form
is called an `abstraction`. We can also call it a `polymorphic form` since it
is a representation of many different concrete forms.

The variable parameters of an abstract form can be replaced with specific
values to recover the original concrete form. By using different values of the
parameters we can recover all possible concrete forms represented by the
abstract form. We have essentially separated the common part from variable
parts allowing us to reuse the common part.

There are different levels of abstractions. An abstract form can be further
abstracted creating higher level abstractions. The fundamental abstraction in
Haskell is a `function`. A function abstracts many values and therefore can
also be thought of as a polymorphic value. A function that can work upon many
different data types is an abstraction of many functions also known as a
`polymorphic function`.

The two fundamental aspects of Haskell that bestow power upon the
programmer are the powerful `abstraction` and `composition` facilities.

Values: Data and Functions
--------------------------

We use the term `value` in a general sense to represent either a `function` or
`data`. A function is an `abstract value` whereas data represents a `concrete
value`. A data is like a container that can hold a value, abstract or concrete.
Note that the data itself is always concrete even though it may be holding an
abstract value inside it.

Essentially, everything in a Haskell program is a value, either function or
data.  A program is nothing but the definition and composition of values.
Functions are not special; they are just an abstract form of some concrete or
abstract values. Functions can be converted back and forth into more abstract
or less abstract forms by the reciprocal processes of `abstraction` and
`reduction`.  Since there is no clear distinction between functions and data
they are processed in similar ways. Hopefully this generalised description will
be clearer after going through the following sections.

::

  Concrete |-------> abstraction    ------>| abstract value
  Value    |<------- reduction or   <------| or function
                 function application

Abstraction of Values
---------------------

+-----------------------------------------------------------------------------+
| Data or concrete values are represented by an expression that has no        |
| unknown parameters in it. Here is a definition representing a concrete      |
| value:                                                                      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  c = 10 + 20                                                                |
+-----------------------------------------------------------------------------+

For illustrations we will represent a concrete value with a complete rectangle:

::

     +----------+
     |          |
  v0 |          |
     |          |
     |    10    |
     +----------+

+-----------------------------------------------------------------------------+
| Abstracting a concrete value creates an `abstract value`,                   |
| `polymorphic value`, `parameterized value` or simply a `function`.          |
+-------------------------------+---------------------------------------------+
| concrete value                | ``c0       = 10 + 10 + 10``                 |
+-------------------------------+---------------------------------------------+
| abstract value of arity 1  | ``f1 a     = a  + 10 + 10``                    |
+-------------------------------+---------------------------------------------+
| abstract value of arity 2  | ``f2 a b   = a  + b  + 10``                    |
+-------------------------------+---------------------------------------------+
| abstract value of arity 3  | ``f3 a b c = a  + b  + c``                     |
+-------------------------------+---------------------------------------------+
| `Arity` is the number of parameters of an abstract value. It is a measure   |
| of abstraction. Higher arity means there are more abstract parameters in    |
| the value.                                                                  |
+-----------------------------------------------------------------------------+
| The abstracted expression `a + b + c` can be `reused` in place of many      |
| concrete values by supplying appropriate values of free variables `a`, `b`  |
| and `c`.                                                                    |
+-----------------------------------------------------------------------------+
| Informally an abstract value can also be thought of as an `incomplete value`|
| which becomes complete or concrete when the real values of abstract         |
| variables are applied to it.                                                |
+-----------------------------------------------------------------------------+
| This abstraction process is also called `beta abstraction` in lambda        |
| calculus terminology. Writing a program is a process of abstraction that    |
| the programmer goes through. The functions defined in a program are a       |
| result of that abstraction.                                                 |
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
| The abstract value represented by the function can be instantiated into a   |
| concrete instance by a `function application` (function call). A function   |
| application would supply the values of parameters as arguments.             |
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

Reduction of Abstract values
----------------------------

+-----------------------------------------------------------------------------+
| Reduction is a process which is opposite of abstraction. A `function        |
| application` concretizes or reduces the abstract value represented by a     |
| function by `combining` the function with concrete values corresponding to  |
| the abstracted parameters.                                                  |
+-----------------------------------------------------------------------------+
| Reduction can be viewed as a `transformation` of a single value or          |
| `composition` of multiple values.                                           |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Reduction                                                                   |
+=============================================================================+
| A function application reduces the arity of the function just as an         |
| abstraction increased the arity of the abstract value.                      |
+----------------------+------------------+-----------------------------------+
| function of arity 3  | f3               | ``f3 a b c = a  + b  + c``        |
+----------------------+------------------+-----------------------------------+
| function of arity 2  | f2 = f3 10       | ``f2 b c = 10  + b  + c``         |
+----------------------+------------------+-----------------------------------+
| function of arity 1  | f1 = f2 10       | ``f1 c = 10  + 10  + c``          |
+----------------------+------------------+-----------------------------------+
| concrete value       | c0 = f1 10       | ``10  + 10  + 10``                |
+----------------------+------------------+-----------------------------------+
| Each application results in another value (function) of reduced arity       |
| finally yielding a concrete value.                                          |
+-----------------------------------------------------------------------------+
| We can also apply multiple arguments at a time:                             |
+----------------------+------------------+-----------------------------------+
| function of arity 1  | f1 = f3 10 10    | ``f1 c = 10  + 10  + c``          |
+----------------------+------------------+-----------------------------------+
| concrete value       | c0 = f3 10 10 10 | ``c0 = 10  + 10  + 10``           |
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

Mathematical Definition of a Function
-------------------------------------

Earlier we described a function as an abstract value.  Another way of a looking
at a function is as a mapping from the values of input parameters to the output
values of the function. In other words, a function discriminates its inputs and
maps them to different output values.

Value Types: Ensuring the Correctness of a Program
--------------------------------------------------

A common mistake in `untyped` or weakly typed languages is to use an `orange`
in a computation where we were supposed to use an `apple`.  In our program, how
do we make sure that we do not supply oranges as input to a function
that only works correctly with apples?

In Haskell, functions are the `only way` to consume and produce values.
Therefore, at compile time if we can check that we are passing an orange to a
function that expects an orange we can avoid this major class of programming
errors.

`Every value` (function or data) in Haskell has a `type` associated with it.
Type is a label that determines the legal values that the data can assume.
Type labels are either explicitly specified by the programmer using `type
signatures` (also known as `type annotations`) or determined automatically by
way of `type inference`. At compile time the type annotations are interpreted
by the typechecker. The typechecker makes sure that when we apply a function to
an input value the type label of that value matches the type label of the
function input. Therefore if a value is labeled `apple` the typechecker will
refuse to pass the data level program if we feed this value to a function input
which is labeled `orange`.

Value Type Signatures
~~~~~~~~~~~~~~~~~~~~~

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
| v = 33                          | Data Level Program (value equation)       |
+---------------------------------+-------------------------------------------+
| Identifier `v` represents the value ``33`` of type ``Int``.                 |
| `Data level program` uses an `=` to bind an identifier to a value while the |
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
| f a b c = ...                   | Data Level Program                        |
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

* `case`: A case expression maps one type to another.  All the values `mapped
  from` must have the same type and all the values `mapped to` must have the
  same type.

* `Equations`: When two values can be substituted in place of each other then
  they must have the same type.

Data & Type Level Programming
-----------------------------

A Haskell program essentially defines computation logic to process input data
and produce output data.  This logic is defined in terms of function
applications. We will call this program the `data level program`. Along with
the data level program a Haskell program also contains a `type level program`
that ensures the correctness of the compute level program at compile time.  The
type annotations for values in a `data level program` can collectively be
regarded as a `type level program`.  We will talk about the basics of a type
level program in the next section.

Data Level Program
------------------

Expressions
~~~~~~~~~~~

There are two fundamental atoms of an expression, function and data.  An
expression represents either a function or data. An expression may consist
of:

+---------------------------------------+-------------------------------------+
| Primitive data                        | Function name                       |
+---------------------------------------+-------------------------------------+
| Data constructor application          | Function application                |
+---------------------------------------+-------------------------------------+
| A data contructor or function can in turn refer to an expression.           |
+-----------------------------------------------------------------------------+

An expression is named (or defined) by an equation:

+-----------------------------------------------------------------------------+
| ``v = 10``                                                                  |
+-----------------------------------------------------------------------------+

Functions & Data
~~~~~~~~~~~~~~~~

Function and data are two fundamental concepts in the construction of a
program. Whenever we say data here we mean alegbraic data.

+---------------------------------------+-------------------------------------+
| Functions                             | Algebraic Data Structures           |
+=======================================+=====================================+
| Abstractions of functions or data     | Containers of functions or data     |
+---------------------------------------+-------------------------------------+
| Created by function definitions or    | Created by data definitions or      |
| function applications                 | constructor applications            |
+---------------------------------------+-------------------------------------+

Note that a function application can generate either data or function whereas a
constructor application always generates data. Though the data may contain a
function.

+---------------------------------------+-------------------------------------+
| Function Definition                   | Algebraic Data Structure            |
|                                       | Definition                          |
+---------------------------------------+-------------------------------------+
| ``f x = x + 10``                      | ``data Color = Red | Green | Blue`` |
+---------------------------------------+-------------------------------------+

Functions as Transformations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function is a transformation that has one or more inputs and precisely one
output. However a multi-input function can be represented as a single input
function that produces a function consuming the rest of the inputs.
Therefore, fundamentally a function can be considered as a transformation with
precisely one input and one output.

Here is an example of a simple function that consumes ``a`` and produces ``b``.
Often, we also say that it is a consumer of ``a`` and producer of ``b``.

+----------+--------+-------+--------+--------+
| function |        | input |        | output |
+----------+--------+-------+--------+--------+
|  ``f``   | ``::`` | ``a`` | ``->`` | ``b``  |
+----------+--------+-------+--------+--------+

Note that the input as well as the output could be data or function.

Higher Arity/Order Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A higher arity function produces a function as output and a higher order
function accepts a function as input.

An n-ary function provides a way to the values of parameters to
in the function definition expression::

  -- multi-arity functions, producing a function as output
  -- nesting of functions on the output side
  -- we will refer to the rank as "arity", arity is 3 in this example
  f :: a -> (b -> (c -> d))

A higher order function provides a way to plug pipes (inputs and outputs)
in the function definition expression::

  -- accepting a function as input
  -- nesting of functions on the input side
  -- we will refer to the rank as "order", order is 3 in this example
  f :: ((a -> b) -> c) -> d

Note that ``->`` is right associative and therefore ``f :: a -> (b -> c)`` is
the same as ``f :: a -> b -> c``. However ``f :: (a -> b) -> c`` is entirely
different, it accepts one argument which is a function.

Operations on Functions
-----------------------

In this section we will look at ways to combine functions and values together.
There are three fundamental ways to combine functions and values:

* `Composition`: When the input type of a function matches the output type of
  another function, the two functions can be chained together by feeding the
  output of the latter to the input of the former::

    -- the arity of the composed function is at least n1 + n2 - 1
    -- output modification, same order, arity
    f :: a -> b
    g :: b -> c
    k :: a -> c
    k = f . g

     -- input modification, same order, arity
     f :: a -> b
     g :: c -> a
     k :: c -> b
     k = f . g

* `Composition`:: Composing functions where the input of one of them is a
  function (higher order function)::

     -- the order of the combined function is at most max (n1, n2)

     f :: a -> (b -> c)
     g :: (b -> c) -> d

     k :: a -> d
     k x = g (f x)

* `Application` or `Currying`: A value matching one of the inputs of a function
  can be fed to the function to generate a lower order function or a data
  value::

    -- reduces the arity
    f :: a -> b -> c
    x :: a
    f x :: b -> c

    f :: (a -> b) -> c
    x :: b
    g :: b -> c
    g x = f (\_ -> x)

* `Extension`: Like an application reduces the arity, an extension increase the
  order of a function. A function and a value can be used such that the input
  of the function is modified to accept a function whose output matches the
  input of original function::

     -- increases the order
     f :: a -> b
     x :: c
     g :: (c -> a) -> b
     g k = f (k x)

Currying first order functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currying refers to function application in multi-arity functions.
Consider this function definition::

  f :: a -> b -> c -> d
  f :: a -> (b -> (c -> d))

We can supply any combination of arguments to this function and leave others
unsatisfied. For regular function currying the arguments must be fed in order,
if we need to curry arguments out of order then we need to make a new function
using a lambda or otherwise. Assume that we have values `x`, `y` and `z` in
scope to be used for parameters `a`, `b` and `c` respectively.

+-----------------+-----------------+-------------+---------------------------+
| consumed (-ve)  | produced (+ve)  | Curry       | Lambda                    |
+=================+=================+=============+===========================+
| a               | (b -> (c -> d)) | f x         | \b c -> f x b c           |
+-----------------+-----------------+-------------+---------------------------+
| b               | a -> c -> d     |             | \a c -> f a y c           |
+-----------------+-----------------+-------------+---------------------------+
| c               | a -> b -> d     |             | \a b -> f a b z           |
+-----------------+-----------------+-------------+---------------------------+
| a, b            | (c -> d)        | f x y       | \c -> f x y c             |
+-----------------+-----------------+-------------+---------------------------+
| b, c            | a -> d          |             | \a -> f a y z             |
+-----------------+-----------------+-------------+---------------------------+
| a, c            | b -> d          |             | \b -> f x b z             |
+-----------------+-----------------+-------------+---------------------------+
| a, b, c         | d               | f x y z     | f x y z                   |
+-----------------+-----------------+-------------+---------------------------+

In any of the productions positive or negative status of `a`, `b`, `c` & `d`
never changes. The regular function application provides us only three ways out
of the seven possible ways to consume inputs.

Currying Higher-order functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function which takes another function as an argument is a higher order
function.

Consider this function::

  f :: (a -> b) -> c
       g^^^^^^^                -- Positive position
        -                      -- Negative position

The function `a -> b` consumes an `a` and produces a `b`. `f` does direct
opposite, it produces that `a` and consumes the `b`. This reversal is
important to keep in mind and becomes even more important when we try to
understand higher order function with even deeper nesting. Every nesting level
flips the consumed or produced roles of the arguments of the function.

+---------------------------------+------------------------+
| Supplied by user, consumed by f | Supplied by f          |
+=================================+========================+
| g :: a -> b                     | x :: a                 |
+---------------------------------+------------------------+

Example: Two level nesting
^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  f :: ((a -> b) -> c) -> d
       g^^^^^^^^^^^^^^           -- Positive position
        --------                 -- Negative position
         x                       -- Positive position

This function is fully applied by supplying two arguments, for example `f g x`.
To understand this it is useful to think in terms of which function is provided
by us and which function is supplied by f.

+---------------------------+------------------------+
| Consumed by f             | Supplied by f          |
+===========================+========================+
| g :: (a -> b) -> c        | k :: a -> b            |
+---------------------------+------------------------+
| x :: a                    |                        |
+---------------------------+------------------------+

We can curry the functions that are supplied by `f` by applying them partially
to the arguments that are supplied by us.

+------------------------+------------------------+---------------------------+
| input                  | Output                 | Example                   |
+========================+========================+===========================+
| g :: (a -> b) -> c     | a -> d                 | f g                       |
+------------------------+------------------------+---------------------------+
| x :: a                 | (b -> c) -> d          | \bc -> f (\k -> bc (k x)) |
+------------------------+------------------------+---------------------------+
| g :: (a -> b) -> c,    | d                      | f g x                     |
| x :: a                 |                        |                           |
+------------------------+------------------------+---------------------------+

See `liftBaseWith` and `defaultLiftWith` for real examples.

Example: Three level nesting
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now lets take an example of a function with three nesting levels.

::

  f :: (((a -> b) -> c) -> d) -> e
       g^^^^^^^^^^^^^^^^^^^^^               -- Positive position
        ---------------                     -- Negative position
         h^^^^^^^                           -- Positive
          -                                 -- Negative

This function is fully applied by supplying two arguments, for example `f g h`.

+---------------------------+------------------------+
| Consumed by f             | Supplied by f          |
+===========================+========================+
| g :: ((a -> b) -> c) -> d | k :: (a -> b) -> c     |
+---------------------------+------------------------+
| h :: a -> b               | x :: a                 |
+---------------------------+------------------------+

We can curry the functions that are supplied by `f` by applying them partially
to the arguments that are supplied by us.

+------------------------+------------------------+---------------------------+
| Consumed by f          | Supplied by f          | Example                   |
+========================+========================+===========================+
| ((a -> b) -> c) -> d   | (a -> b) -> e          | f g                       |
+------------------------+------------------------+---------------------------+
| a -> b                 | (c -> d) -> e          | \cd -> f (\k -> cd (k h)) |
+------------------------+------------------------+---------------------------+
| ((a -> b) -> c) -> d,  | e                      | f g h                     |
| a -> b                 |                        |                           |
+------------------------+------------------------+---------------------------+

Nesting with Currying
^^^^^^^^^^^^^^^^^^^^^

::

  f :: (((a -> b) -> c) -> d) -> m -> e -- f g x h
       g^^^^^^^^^^^^^^^^^^^^^    x
        ---------------
         h^^^^^^^
  f :: m -> (((a -> b) -> c) -> d) -> e -- f x g h

Positive and Negative Positions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is easier to understand this by using a positive and negative position
terminology. What a function consumes (consumable) is negative position and
what it produces (product) is positive position (mnemonic - produce and positive
both start with p). Now, (a -> b) is in negative position in f and a is in
negative position in 'a -> b', it follows a multiplication rule and ``negative
x negative`` becomes positive, therefore `a` is in positive position in `f`.
Similarly, `b` is in negative position in `f` and is therefore consumed by `f`.

Extensions
~~~~~~~~~~

* XXX This section needs to be cleaned up.

Extensions are higher order functions.  A continuation is an interesting
extension.

::

  cont :: (a -> r) -> r

``a -> r`` is a missing piece in this computation which is supplied later. The
missing piece is what produces the final result.

A continuation has already decided the final result (``r``) type of the
computation, it also has an intermediate value ``a``. What it needs is a
function that cosumes the intermediate value and generates a result type which
may be consumed by ``cont`` to generate the final result. The continuation ``a
-> r`` is sort of sandwiched somewhere inside ``cont``.

From a CPS perspective, ($ 2) is a suspended computation: a function with
general type (a -> r) -> r which, given another function as argument, produces
a final result. The a -> r argument is the continuation; it specifies how the
computation will be brought to a conclusion.
Note that suspended computations are largely interchangeable with plain values:
flip ($) [1] converts any value into a suspended computation, and passing id as
its continuation gives back the original value.

When we apply a function, we say that the function consumes the value. However,
a function application is a complementing operation and we can flip the
perspective and say that the value is eaten by some function instead. ``flip
($)`` flips the value into a function which eats some function to complete the
application. Or we can say that we wrapped the value into a higher order match
maker function which has eaten one part of the match and is waiting for the
other part. Continuations create holes in a computation to be filled later, it
is an incomplete or suspended computation.

Continuation is just a dual of the function application. They are just another
way of composing - in the opposite direction. We just have to think from the
end to the beginning rather than the other way round.

You have f, you pass it a value, the value is - you have g and you pass x to
it::

  f (g x)

You have x, it is to be fed to someone (g) and that in turn is to be fed to
someone else (f)::

  \f -> f y
  \g -> g x

A continuation is a reverse function application style. In a continuation we
say that this value is to be used by someone, say k. In a forward application
style we say this function will be applied to some value.

https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style pythgoras
example.

In fact a continuation passing style is a more straightforward thinking. For
example::

  pythagoras_cps x y = \k ->
  square_cps x $ \x_squared ->
  square_cps y $ \y_squared -> -- square y and the pass the result to second arg
  add_cps x_squared y_squared $ k -- add two values and pass the result to k

Here we say, square x, then square y, then add them and then pass the result to
k. In contrast see the regular function application style::

  pythagoras x y = add (square x) (square y)

we are saying, add two things, first thing is a square of x, the second thing
is a square of y.

Both ways are equivalent, just a dual of each other. In continuation style a
value is provided and we need who eats it i.e. the continuation of this value.

The Cont monad makes composing the continuations much easier. Basically it
allows us to write the continuations in the straight application style::

  pythagoras_cont :: Int -> Int -> Cont r Int
  pythagoras_cont x y = do
      x_squared <- return (square x)  -- perform square of x, use it later
      y_squared <- return (square y)  -- perform square of y use it later
      return (x_squared + y_squared)  -- add the squares, use the result later

Cont monad straightens the callback style programming. A continuation can be
thought of as a callback. In a callback style "square x" can take a callback
and call it when it is done squaring x. In a continuation style the rest of the
computation is the callback or continuation of "square x" though written in a
straightforward manner because all the callbacks are lined up sequentially.

Event driven programming is suited to a cont monad. Event driven programming
and upfront available value driven programming are duals of each other. In
regular programming we have all the values available and compute using that. In
event driven programming values are generated by events and when it is
generated we need to pass it to the consumer, this is reverse style. In the
same way cont monad is a dual of the regular straightforward funciton
applicaiton style.

A more general, MachineT example::

  The CPS form is:

  newtype PlanT k o m a = PlanT
    { runPlanT :: forall r.
        (a -> m r) ->                                     -- Done a
        (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
        (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
        m r ->                                            -- Fail
        m r
    }

runPlanT is a computation that takes multiple missing pieces. The PlanT monad
allows us to compose a computation and then we can supply these missing pieces
later to complete the computation. The missing pieces are all continuations as
their result type is the same as the result type of the whole computation.

::

  runPlanT :: forall r. (a -> m r) -> (o -> m r -> m r) -> (forall z. (z -> m r)
  -> k z -> m r -> m r) -> m r -> m r

  The CPS form is equivalent to the following regular form:

  data Plan k o a
    = Done a              -- runPlanT supplies a to a -> m r
    | Yield o (Plan k o a) -- runPlanT supplies o and m r to (o -> m r -> m r)
    | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
    | Fail

Kan Extensions
~~~~~~~~~~~~~~

::

  -- Right Kan Extension
  newtype Ran g h a = Ran (forall b. (a -> g b) -> h b)

  -- Left Kan Extension
  data Lan g h a = Lan (forall b. (g b -> a) (h b))

* http://comonad.com/reader/2008/kan-extensions/

Codensity
~~~~~~~~~

A special case of right Kan Extension where g and h are the same::

  newtype Codensity m a = Codensity (forall b. (a -> m b) -> m b)

* Reference: Asymptotic Improvement of Computations over Free Monads

Yoneda
~~~~~~

::

  type Yoneda = Ran Identity
  newtype Yoneda m a = Yoneda (forall b. (a -> b) -> m b)

* http://blog.sigfpe.com/2006/11/yoneda-lemma.html
* http://www.math.harvard.edu/~mazur/preprints/when_is_one.pdf When is one
  thing equal to some other thing?


Defining Functions
------------------

Functions Defined Purely in Terms of Compositions, Applications or Extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Composed functions are expressions defined purely in terms of composed
applications of other functions. They pass on their arguments without having to
know their values and hence do not discriminate the logic based on them.  In
other words, they treat their parameters as opaque data.  It means that they do
not need to de-construct the algebraic structure of their arguments.

::

  square x = x * x

This classification is not very interesting as such but it is a value level
equivalent of function-level parametric polymorphism at the type level. Such
functions do not discriminate values the way parametrically polymorphic type
functions do not discriminate types. We can say that a composed function is a
parametrically polymorphic value.

Ad-hoc Functions
~~~~~~~~~~~~~~~~

As opposed to composed functions which transform data by just composing other
functions, ad-hoc functions de-construct the algebraic structure of their
arguments by using case analysis and map input values to custom output
values.

The following example de-structures the parameter ``x`` and maps specific
values (numbers) to specific outputs (number names):

::

  name x =
        case x of
          1 -> "one"
          2 -> "two"
          3 -> "three"

Note how this looks very similar to a mathematical definition of a function.
We call this an ad-hoc function as it is a custom or user defined function.
This is analogous to the way ad-hoc polymorphism defines a custom function for
each type at function level (typeclasses).

Ad-hoc functions require a knowledge of the structure of the algebraic data to
de-structure it. A `data declaration` defines the structure of an algebraic
data type in terms of existing data types, it creates a new `type` at the type
level representing the data structure. It also creates `data constructor`
functions for the type to instantiate it in the data level program. Case
analysis is used to de-structure the data.

+--------------------------+---------------------+----------------------------+
| Data Level               | Bridge              | Type Level                 |
+==========================+=====================+============================+
| Data constructors        |                     |                            |
+--------------------------+                     |                            |
| Case analysis            | Data declaration    |                            |
| (Ad-hoc Function)        |                     | Algebraic Data Types       |
+--------------------------+---------------------+----------------------------+

Algebraic Data Types
^^^^^^^^^^^^^^^^^^^^

+----------------------------+
| Algebraic Data Types (ADT) |
+-----+---------+------------+
| Sum | Product | Recursive  |
+-----+---------+------------+

A type represents a number of choices or values. For example, an `Int` type
represents 2^64 choices on a 64 bit machine each representing a different
number. Let us define a `Color` data type representing three colors:

::

  data Color = Red | Green | Blue -- 3 values

This is a primitive algebraic data type since it is not defined in terms of
any other algebraic data types.  `Color` is a `sum` type as the total number of
choices are the sum of individual choices (1 + 1 + 1). A primitive algebraic
data type is always a sum type since it is an enumeration all the choices
represented by the data type.

Another example is a `Size` data type with two values:

::

  data Size = Tiny | Big -- 2 values

We can build composite algebraic data types by defining a new type as a `sum`
or `product` of existing types. For example `Properties` of an object can be
defined as a sum of `Color` and `Size`:

::

  data Properties = P1 Color | P2 Size -- 3 + 2 = 5 values

This is a sum type which represents all 5 properties i.e. 3 colors and 2 sizes
of an object. Similarly we can describe an object with its color and size:

::

  data Object = Object Color Size -- 2x3 = 6 values

Here we are saying that an `Object` type is a combination of color and size.
Since a `Color` has 3 possibilities and a `Size` has 2 possibilities, the type
`Object` has 6 distinct combinations:

+---------------+
| Object        |
+-------+-------+
| Red   | Tiny  |
+-------+-------+
| Red   | Big   |
+-------+-------+
| Green | Tiny  |
+-------+-------+
| Green | Big   |
+-------+-------+
| Blue  | Tiny  |
+-------+-------+
| Blue  | Big   |
+-------+-------+

The data type `Object` therefore represents a total of 6 possible choices or
values.  The total choices represented by `Object` is a product of the choices
represented by `Color` and `Size` i.e. 3x2. That's why it is called a product
type. An `Object` therefore is a product of two sum types.

Let us now build a `Shape` data type. A shape could be a triangle or a square.
For each shape we also describe its color and size.

::

  data Shape = Triangle Color Size | Square Color Size   -- 3x2 + 3x2 = 12

This data type is a sum of products where each product is built using a `Color`
and a `Size`. `Shape` describes a total of 12 values.

If we represent a type as a box we can visually represent each value of `Shape`
as nested boxes. For example a `Red Tiny Triangle` can be visualized as:

TBD - picture

Algebraic data types can be defined recursively. For example a list of `Int`
can be defined as:

::

  data List = Empty | Cons Int List

  +--------+-------+      +--------+-------+      +--------+
  |  100   |  n2   |----->|  200   |  n1   |----->|  Empty |
  +--------+-------+      +--------+-------+      +--------+

All types in Haskell are defined either as primitive sum types or composite
types defined in terms of sums and products of other types.

The product data type is somewhat analogous to `record` types (e.g. `struct` in
C) in other languages. Similarly a sum type is analogous to enumerated types
(e.g. `enum` in C).

Constructing Algebraic Data
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Physical Representation of ADTs
+++++++++++++++++++++++++++++++

TBD

Data Constructors
+++++++++++++++++

+-----------------------------------------------------------------------------+
| `Data construction:` A data value is always created by applying a           |
| constructor function on some existing values (function or concrete data).   |
+-----------------------------------------------------------------------------+
| v = Triple 1 2 3                                                            |
+-----------------------------------------------------------------------------+
| c = Red                                                                     |
+-----------------------------------------------------------------------------+
| `built-in data` literals (e.g. 3, "name") are just a syntactic sugar        |
| ultimately implemented with data constructor applications.                  |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| A `data constructor` is a function                                          |
| which maps its argument data items to a new output data item.               |
| The definition of the function is automatically generated by the compiler   |
| according to the user specified data declarations. The function essentially |
| creates a reference to an object which points to the contained objects      |
+-----------------------------------------------------------------------------+
| Triple a b c = <compiler defined>                                           |
+-----------------------------------------------------------------------------+
| When a data type represents one of multiple options,                        |
| each option is represented by a separate constructor function.              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  Red = <compiler defined>                                                   |
|  Green = <compiler defined>                                                 |
|  Blue = <compiler defined>                                                  |
+-----------------------------------------------------------------------------+
| The name of a data constructor must start with an upper case letter.        |
+-----------------------------------------------------------------------------+

Data Declaration
^^^^^^^^^^^^^^^^

Data constructor function definitions are supplied by the compiler based on the
signatures specified by the programmer through a data declaration. A data
declaration specifies a data type on the LHS and constructor templates on the
RHS.

+---------------------------------------------------------+-----------------------------------------------+
| data declaration                                        | Equivalent data constructor signatures        |
+=========================================================+===============================================+
| data Color = :blue:`Red` | :blue:`Green` | :blue:`Blue` | :blue:`Red` :: Color                          |
|                                                         +-----------------------------------------------+
|                                                         | :blue:`Green` :: Color                        |
|                                                         +-----------------------------------------------+
|                                                         | :blue:`Blue` :: Color                         |
+---------------------------------------------------------+-----------------------------------------------+
| data Triple = :blue:`Triple` Int Int Int                | :blue:`Triple` :: Int -> Int -> Int -> Triple |
+---------------------------------------------------------+-----------------------------------------------+
| Blue color identifiers are data level identifiers while the rest is type level.                         |
+---------------------------------------------------------+-----------------------------------------------+

GADT syntax is a way of specifying the constructor signatures directly.

Case Analysis
^^^^^^^^^^^^^

Deconstructing Data By Pattern Matching
+++++++++++++++++++++++++++++++++++++++

+-----------------------------------------------------------------------------+
| `pattern match` is exact opposite of data construction, it de-constructs a  |
| data value into its components. It is a constructor application on the      |
| LHS of an equation with variables as arguments. The variables get bound to  |
| the respective components of the data on RHS.                               |
+-----------------------------------------------------------------------------+
| Triple a b c = v -- a, b and c get bound to the individual components of    |
| the pair                                                                    |
+-----------------------------------------------------------------------------+
| Blue = c -- will fail if the value c was constructed using Red for example  |
+-----------------------------------------------------------------------------+
| When there are multiple constructors.                                       |
| If the pattern specified does not match with the data value, the pattern    |
| match fails.                                                                |
+-----------------------------------------------------------------------------+

Implementing a Function using `case`
++++++++++++++++++++++++++++++++++++

-- use case n of 1, 2, 3 etc.

+-----------------------------------------------------------------------------+
| We said a function maps input values to output values, how exactly does it  |
| do it?  The fundamental primitive to achieve that is a `case pattern        |
| match`.  A case statement can enumerate all patterns for an input value and |
| maps them to specified output values. Case is essentially a value to value  |
| map.                                                                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  case color of                                                              |
|    Red   -> "red"                                                           |
|    Green -> "green"                                                         |
|    Blue  -> "blue"                                                          |
+-----------------------------------------------------------------------------+
| When the value `color` is `Red` this expression will evaluate to            |
| `"red"`                                                                     |
+-----------------------------------------------------------------------------+
| A case statement is the fundamental tool to define a function from a value  |
| of one type to a value of another type. A case implements the mathematical  |
| definition of a function. A case expression takes one type and outputs      |
| another.                                                                    |
+-----------------------------------------------------------------------------+

Case statements and function applications can be combined together to form a
chain of transformations to arrive at the final output value of a function.
Thus `data constructors`, `case pattern match` and `function application` are
really the basic building blocks of a Haskell program. `All functions can be
expressed in terms of these primitives`. A function application (and case) are
the only primitives that can map a value of one type to another type.

Type Level Programming
----------------------

The purpose of type level programming is to generate concrete types to be used
in the data level program.  Just like at data level we create `data functions`
representing `asbtract` or `polymorphic data`, the same way at the type level
we can create `type functions` representing abstract or `polymorphic types`.
Type functions can be used to compose types together to generate more complex
types from simple concrete types.

Note that the type assigned to any data level value is always `concrete`.  The
type of a data value can never be a type function. Type functions only exist at
the type level. See the kinds section for details.

Type Inference
~~~~~~~~~~~~~~

Every value in Haskell has a type associcated with it. A type originates when
you define a data via a `data declaration`. A data declaration is in fact a
specification of the type of the data. In theory this is the only place where
the programmer has to specify a type, all other types in the program can be
`inferred` from this.

The type of a function can be inferred from the use of the function in the
program. The number of arguments that a function takes can be inferred from its
use at the use sites. Each use of the function must consistently agree with the
number of arguments it takes. Similarly the types of its arguments can be
inferred from the types of arguments passed to it at the use sites. The return
value type of a function can also be inferred from the use site by looking at
the type of the data item to which the return value is assigned. If the type
of a function is inferred to be different at different places the typechecker
will complain.

An expression is essentially either a data value or a function call. Therefore,
the type of an expression can be inferred the same way as a function.

In a pattern match the type of a deconstructed component can be inferred from
the type of the component in the data element being deconstructed.

Type Signatures
~~~~~~~~~~~~~~~

However, there may be situations where the inferred type may be ambiguous. In
such cases, the programmer can provide type annotations or type signatures to
remove the ambiguity. Also, it is recommended to specify type signatures for
all top level declarations it helps in diagnosing the type errors. One way to
narrow down type errors is by specifying type signatures to the known types
involved in an expression.

A programmer can specify type signatures at the following places:

* declarations - function definitions, let or where clauses
* expressions - any part of an expression can be given a type
* pattern matches

Generating function types
~~~~~~~~~~~~~~~~~~~~~~~~~

What is the type of a function value? A function with one argument is different
from a function with two arguments. A function accepting an `Int` argument is
different from a function accepting `Char` argument. The same applies to return
values as well. The combinations are huge, so how do we represent so many types
uniquely?

+-----------------------------------------------------------------------------+
| We generate function types using a type level operator denoted by           |
| ``->``. A function of multiple arguments is represented by consuming one    |
| argument at a time.                                                         |
+------------------+----------------------------------------------------------+
| (->) a b         | Returns a concrete type representing a data function     |
+------------------+ which takes a data level value of type `a` as argument   |
| a -> b           | and returns a data level value of type `b`.              |
+------------------+----------------------------------------------------------+
| (->) a ((->) b c)| Returns a concrete type representing a data function     |
+------------------+ which takes a data value of type `a` as argument and     |
| a -> (b -> c)    | returns a function of type ``(b -> c)``, note that the   |
+------------------+ operator is right associative.                           |
| a -> b -> c      |                                                          |
+------------------+----------------------------------------------------------+
| For example                                                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  add :: (->) Int ((->) Int Int)  -- function form                           |
|  add :: Int -> (Int -> Int)      -- explicit right associative form         |
|  add :: Int -> Int -> Int        -- commonly used infix form                |
|  add x y = x + y                                                            |
+-----------------------------------------------------------------------------+

TBD - deduplicate this with the table in the syntax chapter

Parametric Polymorphism
-----------------------

When the parameters of a function are of a variable type i.e. polymorphic then
the function is known as parametrically polymorphic function.

Polymorphic Types
~~~~~~~~~~~~~~~~~

A parametrically polymorphic type is a type function parameterized by a type
variable (``a`` in the following example)::

  data Pair a = Pair a a

The type can be `instantiated` for a specific value of the variable `a`, for
example the type ``Pair Int`` is equivalent to the definition ``Pair Int Int``.

Polymorphic Functions
~~~~~~~~~~~~~~~~~~~~~

The arguments and/or return value of a parametrically polymorphic function can
be a variable type. The function can be `instantiated` for any value of the
type variable::

  id :: a -> a
  id x = x

The `a` in the signature of this function is a `type variable`. `a` can assume
any concrete type.

Scope and Quantification of Type Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type variables in a type signature lexically scope over the whole type
signature. However, unless ``ScopedTypeVariables`` is enabled, they are not
visible to any type signatures inside the declararion.

The type variables in a function signature are by default `universally
quantified`. You can think of quantification as scoping from the typechecker
perspective. Universal quantification implies that the type variables are
scoped globally across the entire program from the typechecker perspective.
Therefore, when a polymorphic function is `instantiated`, the specific values
of the type variables are determined by the user of the function.  For
example::

  let x = 'a'
  id x -- id :: Char -> Char, because x is of type Char

When a (universally quantified) type variable occurs at more than one places in
a signature it means that both the types are same. For example the argument and
the result type in the following function must be the same::

  id :: a -> a
  id :: Int -> Int
  id :: Char -> Char

+-----------------------------------------------------------------------------+
| A programmer-written type signature is implicitly quantified over its free  |
| type variables.                                                             |
+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XExplicitForAll                                                           |
+-----------------------------------------------------------------------------+
| Allow use of `forall` keyword where universal quantification is implicit.   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  id :: forall a. a -> a                                                     |
|  id :: forall a. (a -> a)                                                   |
|  instance forall a. Eq a => Eq [a] where ...                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XScopedTypeVariables                                                      |
+-----------------------------------------------------------------------------+
| Enable lexical scoping of type variables explicitly introduced with         |
| `forall`. `The type variables bound by a forall` scope over the entire      |
| definition of the accompanying value declaration.                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. [a] -> [a]                                                  |
|  f xs = ys ++ ys                                                            |
|      where                                                                  |
|        ys :: [a]                                                            |
|        ys = reverse xs                                                      |
+-----------------------------------------------------------------------------+
| * A scoped type variable stands for a type variable, and not for a type.    |
| * Distinct lexical type variables stand for distinct type variables         |
| * A lexically scoped type variable can be bound by a declaration,           |
|   expression, pattern type signature and class and instance declarations.   |
+-----------------------------------------------------------------------------+
| * Any type variable that is `in scope` is not universally quantified.       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  (e :: a -> a)     means     (e :: a -> a)                                  |
|  (e :: b -> b)     means     (e :: forall b. b->b)                          |
+-----------------------------------------------------------------------------+
| An expression type signature that has explicit quantification               |
| (using forall) brings into scope the explicitly-quantified type variables,  |
| in the annotated expression. For example:                                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f = runST ( (op >>= \(x :: STRef s Int) -> g x) :: forall s. ST s Bool )   |
+-----------------------------------------------------------------------------+
| Unlike expression and declaration type signatures, pattern type signatures  |
| are not implicitly generalised. The pattern in a pattern binding may only   |
| mention type variables that are already in scope. For example:              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. [a] -> (Int, [a])                                           |
|  f xs = (n, zs)                                                             |
|    where                                                                    |
|      (ys::[a], n) = (reverse xs, length xs) -- OK                           |
|      zs::[a] = xs ++ ys                     -- OK                           |
|                                                                             |
|      Just (v::b) = ...  -- Not OK; b is not in scope                        |
+-----------------------------------------------------------------------------+
| However, in all patterns other than pattern bindings, a pattern type        |
| signature may mention a type variable that is not in scope; in this case,   |
| the signature brings that type variable into scope. This is particularly    |
| important for existential data constructors. For example:                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data T = forall a. MkT [a]                                                 |
|                                                                             |
|  k :: T -> T                                                                |
|  k (MkT [t::a]) =                                                           |
|      MkT t3                                                                 |
|    where                                                                    |
|      t3::[a] = [t,t,t]                                                      |
+-----------------------------------------------------------------------------+
| in this situation (and only then), a pattern type signature can mention a   |
| type variable that is not already in scope; the effect is to bring it       |
| into scope, standing for the existentially-bound type variable.             |
+-----------------------------------------------------------------------------+

Higher Rank Parametric Polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When all the type variables of a function are universally quantified the values
of type variables and therefore the function instance is completely decided by
the caller context. However, when one of the parameters of a polymorphic
function is a function, that function will be called by the polymorphic
function itself. If that function is polymorphic and its type variables are not
universally quantified but scoped to the polymorphic function itself then the
specific instance of that functon is completely decided by the calling
polymorphic function. Essentially the inner function instance depends on how
the outer function is instantiated i.e. how the type parameters of the outer
function are chosen.

Such a polymorphic function that instantiates another polymorphic function
locally depending on its own instance is called a rank-2 polymorphic function.
Similarly if the inner function instantiates another polymorphic function
locally then we get a rank-3 polymorphism and so on.

The scoped quantification is introduced by the ``XRankNTypes`` GHC extension.

+----------------------+--------+--------------------------------------------------------------------------------+-------------------------+
| Polymorphic Functions| Rank3  | ``f :: (Rank2 polymorphic function type) -> b``                                | Abstract functions      |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | f :: ((forall a. a -> a) -> Int) -> Int                                        |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | Rank can be determined by counting the nesting level of the type variable      |                         |
|                      +--------+--------------------------------------------------------------------------------+                         |
|                      | Rank2  | ``f :: (Rank1 polymorphic function type) -> b``                                |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | This function itself may be monomorphic but it accepts a polymorphic function  |                         |
|                      |        | as an argument                                                                 |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | The key point is that the instantiation of the polymorphic function passed as  |                         |
|                      |        | argument is decided by this function.                                          |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | f :: (forall a. a -> a) -> Int                                                 |                         |
|                      +--------+--------------------------------------------------------------------------------+                         |
|                      | Rank1  | ``f :: a -> b`` where type variable `a` represents values of Rank0             |                         |
+----------------------+--------+--------------------------------------------------------------------------------+-------------------------+
| Monomorphic Functions         | ``f :: Char -> Int``                                                           | Concrete function       |
|                               |                                                                                | Abstract value          |
|                               |                                                                                | Polymorphic value       |
+-------------------------------+--------------------------------------------------------------------------------+-------------------------+
| Concrete Data Values          | ``f :: Int``                                                                   | Monomorphic value       |
+-------------------------------+--------------------------------------------------------------------------------+-------------------------+

Any of the type parameters of a function can be made locally quantified by
grouping it with a forall keyword. For example::

  f :: a -> a             -- implicitly universally quantified
  f :: forall a. a -> a   -- explicitly universally quantified

  f :: (forall a. a) -> a -- the first parameter is locally quantified and is
                          -- distinct from the return type variable
  f :: a -> forall a. a   -- the return type is locally quantified and is
                          -- distinct from the first parameter.

+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XRankNTypes                                                               |
+-----------------------------------------------------------------------------+
| Arbitrary-rank polymorphism                                                 |
+-----------------------------------------------------------------------------+
| Rank-1 types                                                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. Ord a => a -> a                                             |
|  f :: Int -> (forall a. a -> a)                                             |
|  f :: Int -> forall a. a -> a                                               |
|  f :: Int -> Ord a => a -> a                                                |
+-----------------------------------------------------------------------------+
| Rank-2 types                                                                |
+-----------------------------------------------------------------------------+
| ``f :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int``                 |
+-----------------------------------------------------------------------------+
| Rank-3 types                                                                |
+-----------------------------------------------------------------------------+
| ``f :: ((forall a. a -> a) -> Int) -> Bool``                                |
+-----------------------------------------------------------------------------+
| Inference                                                                   |
+-----------------------------------------------------------------------------+
| For a lambda-bound or case-bound variable, x, either the programmer         |
| provides an explicit polymorphic type for x, or GHC’s type inference will   |
| assume that x’s type has no foralls in it.                                  |
+-----------------------------------------------------------------------------+

Specialization
~~~~~~~~~~~~~~

TBD

Inlining. Expansion - specialization + inlining.

Type Signatures
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Type Signatures                                                             |
+-----------------------------------------------------------------------------+
| Type signatures can be given to a declaration, expression or a pattern      |
+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XFlexibleContexts                                                         |
+-----------------------------------------------------------------------------+
| Allow flexibility in declaring in type class constraints.                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  g :: Eq [a] => ...                                                         |
|  g :: Ord (T a ()) => ...                                                   |
+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XAllowAmbiguousTypes                                                      |
+-----------------------------------------------------------------------------+
|                                                                             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  TBD                                                                        |
+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XKindSignatures                                                           |
+-----------------------------------------------------------------------------+
| Explicitly-kinded quantification                                            |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  TBD                                                                        |
+-----------------------------------------------------------------------------+

* Bindings and generalisation (TBD)
* Visible type application
* Implicit parameters

* Impredicative polymorphism
* Typed Holes
* Partial Type Signatures

Kinds: Ensuring correctness of Types
------------------------------------

+-----------------------------------------------------------------------------+
| Safety of type level programming is ensured by labeling types with different|
| `kinds` and performing a `kind check` when a type function is applied.      |
| Kinds are relatively few and classified as follows:                         |
+-----------------------------------------------------------------------------+

Primitive Kinds
~~~~~~~~~~~~~~~

.. _RuntimeRep: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#runtime-representation-polymorphism

+--------------------------------------------------+--------------------------+
| Kind                                             | Description              |
+==================================================+==========================+
| ``TYPE 'IntRep'``, ``TYPE 'DoubleRep'`` ...      | Unlifted Types           |
+--------------------------------------------------+--------------------------+
| ``Type`` or ``*`` (``TYPE 'PtrRepLifted'``)      | Lifted Types             |
+--------------------------------------------------+--------------------------+
| ``Constraint``                                   | Typeclass Constraints    |
+--------------------------------------------------+--------------------------+
| ``Nat``                                          | Type level naturals      |
+--------------------------------------------------+--------------------------+
| ``Symbol``                                       | Type level symbols       |
+--------------------------------------------------+--------------------------+
| A concrete type's kind encodes the runtime representation (e.g. unlifted or |
| lifted) of the type.                                                        |
+-----------------------------------------------------------------------------+
| GHC internally represents a type kind as ``TYPE`` parameterised by          |
| `RuntimeRep`_.                                                              |
+-----------------------------------------------------------------------------+

Kind Signatures
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Just like a type signature associates types to a value, a `kind signature`  |
| associates kinds to a type.                                                 |
+-----------------------------------------------------------------------------+

Kinds of Concrete Types
^^^^^^^^^^^^^^^^^^^^^^^

+-----------+------+-------------------+
| Type      |      | Kind              |
+===========+======+===================+
| .. class:: center                    |
|                                      |
| Unlifted Types                       |
+-----------+------+-------------------+
| Int#      | `::` | TYPE 'IntRep'     |
+-----------+------+-------------------+
| Double#   | `::` | TYPE 'DoubleRep'  |
+-----------+------+-------------------+
| Array#    | `::` | TYPE 'ArrayRep'   |
+-----------+------+-------------------+
| .. class:: center                    |
|                                      |
| Lifted Types                         |
+-----------+------+-------------------+
| RealWorld | `::` | Type              |
+-----------+------+-------------------+
| Int       | `::` | Type              |
+-----------+------+-------------------+
| Maybe Int | `::` | Type              |
+-----------+------+-------------------+

Kinds of Type Functions
^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| We generate the kinds of type functions by using the kind level operator    |
| ``->``.                                                                     |
+-----------------------------------------------------------------------------+

+-----------+------+----------------------+
| Type      |      | Kind                 |
+===========+======+======================+
| Maybe     | `::` | Type -> Type         |
+-----------+------+----------------------+
| Either    | `::` | Type -> Type -> Type |
+-----------+------+----------------------+
| (->)      | `::` | Type -> Type -> Type |
+-----------+------+----------------------+

Kind check
~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `Kind check` fails if we pass the wrong kind to a type function.            |
+------------------------------+-------------+--------------------------------+
| Function                     | Application | Failure Reason                 |
+------------------------------+-------------+--------------------------------+
| Maybe :: Type -> Type        | Maybe Int#  | Wrong kind ``TYPE 'IntRep'``   |
|                              |             | expected ``Type``              |
+------------------------------+-------------+                                |
| (->) :: Type -> Type -> Type | Int# -> Int |                                |
+------------------------------+-------------+--------------------------------+

Abstraction Ladders
-------------------

Data & Type Level Bridges
~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------------------+---------------------+------------------------+
| Data Level                   | Connector           | Type Level             |
+==============================+=====================+========================+
| Ad-hoc polymorphism          | Typeclass           | Ad-hoc type functions  |
|                              |                     | (type families)        |
+------------------------------+---------------------+------------------------+
| Ad-hoc functions             | Data declaration    | Algebraic Data Types   |
| (case defined)               | (Data constructors) | (user defined)         |
+------------------------------+---------------------+------------------------+
| Values                       | Type signature      | Concrete types         |
+------------------------------+---------------------+------------------------+

Data Level Abstraction Ladder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------------------------+---------------------------------------+
| Description                         | Example                               |
+=====================================+=======================================+
| Ad-hoc polymorphism                 |                                       |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
|                                     |  f :: a -> a                          |
| Parametrically polymorphic functions|  f x = x                              |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
|                                     |  f :: Int -> String                   |
| Ad-hoc functions                    |  f x = case x of                      |
| (case defined)                      |    1 -> "one"                         |
|                                     |    _ -> "any"                         |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Composed functions                  |  f x y = x + y                        |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Concrete values (expressions)       |  5 + 4                                |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Concrete values (literals)          |  'a', 5, "hello"                      |
+-------------------------------------+---------------------------------------+

Type Level Abstraction Ladder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------------------------+---------------------------------------+
| Description                         | Example                               |
+=====================================+=======================================+
| Polymorphic Type Functions          |                                       |
| (e.g. ``t :: k1 -> k2``)            |                                       |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Ad-hoc type functions               |  data family Pair                     |
| (type families)                     |  Pair () = Pair                       |
|                                     |  Pair Int = Pair Int Int              |
+-------------------------------------+---------------------------------------+
| Type functions (polymorphic ADT)    | ::                                    |
|                                     |                                       |
|                                     |  data Pair a = Pair a a               |
+-------------------------------------+---------------------------------------+
| Concrete/Monomorphic types          | ::                                    |
| (Algebraic Data Types/              |                                       |
| user defined)                       |  data Color = Red | Green | Blue      |
+-------------------------------------+---------------------------------------+
| Concrete/Monomorphic types          | ::                                    |
| (expressions)                       |                                       |
|                                     |  Int -> Int, [Int] ...                |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Concrete/Monomorphic types (basic)  |  Int, Char, ...                       |
+-------------------------------------+---------------------------------------+

Summary of Programming Levels
-----------------------------

+--------------+---------------------------+-------------+----------------------------------------------------+
| When         | What                      | Objects     | Haskell Program Features                           |
+==============+===========================+=============+====================================================+
| Compile time | `Kind` level programming  | Kinds       | Kind Signatures                                    |
|              +---------------------------+-------------+----------------------------------------------------+
|              | `Type` level programming  | Types       | Function Type Signatures                           |
|              |                           |             +----------------------------------------------------+
|              |                           |             | Data declarations (constructor signatures)         |
|              |                           |             +----------------------------------------------------+
|              |                           |             | Typeclasses (Function signatures & Data decl.)     |
+--------------+---------------------------+-------------+----------------------------------------------------+
| Run time     | `Data` level programming  | Data        | Concrete data values, Functions, Data Constructors |
+--------------+---------------------------+-------------+----------------------------------------------------+

General Model of a Haskell Program
----------------------------------

A Haskell program is essentially a function called `main` which `maps` input
`values` of the program to output `values` potentially via intermediate
functions.

If you flatten a Haskell program it can be thought of just as a big function map, each
input decomposed and mapped to intermediate outputs which are again decomposed
and mapped to the next outputs and so on until we get to the final
output.

+-----------------------------------------------------------------------------+
| Haskell Program: Essentially a set of equations defining functions or data  |
+============+================================================================+
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

Namespaces
----------

The names or identifiers in one level (data, type or kind) should not be
confused or conflated with the names in other level. An identifier of the same
name can be used in different levels without any problem.

Tips: Understanding a Haskell Program
-------------------------------------

Names of data constructor functions and types could be the same, which can be
confusing for beginners. Similarly type variables in type level and type
parameters in data level could be same or different, they should not be
confused with each other.

Names, References, Bindings & Scopes
------------------------------------

Names are given to values, functions, function parameters, data constructors or
types so that we can refer to them uniquely in expressions. The LHS of any
`definition` (definition and declaration are used interchangeably in Haskell)
is a `name`. In the following examples `x` is said to be in `binding position`:

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

An expression can either have literals, which are values without a name, or it
can refer to names which are defined elsewhere.  Note, an anonymous function
can be called a function literal since it does not have a name.  Every
`reference` to a name in an expression is `resolved` and  `bound` to some
definition or to a name in a binding position.

Definitions may be nested within other definitions.  A definition which is not
nested in any other definition is a `top level declaration`. Each nest level
creates a `scope`. In a given module, at any given scope, we cannot have
multiple definitions with the same name. However, the same name can be defined
at different scopes. When two scopes in hierarchy define the same name, we
`resolve` the name to innermost scope.  The other definitions of the name are
said to be `shadowed` by the chosen definition. The definition to which the
name reference is bound is said to `capture` the reference.

+-------------------+---------------------------------------------------------+
| ::                | The variable `x` on RHS is captured by or bound to the  |
|                   | parameter `x` of `f`                                    |
|  f x = x          |                                                         |
+-------------------+---------------------------------------------------------+
| ::                | The `x` in `g x` captures the `x` on RHS. The `x` in    |
|                   | `f x` is shadowed by the `x` in `g x`.                  |
|  f x = g          |                                                         |
|     where g x = x |                                                         |
+-------------------+---------------------------------------------------------+

Summary
-------

* A function is really the only building block of Haskell
* A Haskell program is a specification of equations for functions
* There are three independent functional programming spaces viz. data, type
  and kind
* The bridge between any two spaces is a function name

References
----------

* https://en.wikipedia.org/wiki/Scope_(computer_science)
