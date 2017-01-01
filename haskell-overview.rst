.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Haskell: A pure functional language
===================================

A functional language with denotational semantics.

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
| Value                  | A generic term for anything that can accepted as   |
|                        | arguments by a function. It could be a function or |
|                        | data in a data level program or types or type      |
|                        | functions in a type level program.                 |
+------------------------+----------------------------------------------------+
| Parameters             | The free variables in a function's definition      |
+------------------------+----------------------------------------------------+
| Arguments              | Parameter values supplied in a function call       |
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
| Concrete               | Represents a real physical value (not abstract)    |
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


Abstraction & Polymorphism
--------------------------

`Abstraction` or `polymorphism` is a powerful tool to `reuse` code and remove
duplication or redundancy. When we have  multiple `concrete` objects which
have a common structure but differ only slightly in some parts then we can
create an abstract form of the object by retaining the common parts and
abstracting the varying parts by replacing them with variable parameters.  This
form is called an `abstraction` or a `polymorphic form` since it is an abstract
representation of many different concrete forms.

The variable parameters of a polymorphic or abstract form can be replaced with
specific values to recover a concrete form. By using different values of the
parameters we can recover all possible concrete forms.

There are different levels of abstractions. Even an abstract form can be
further abstracted creating higher level abstractions. The first level of
abstraction is a `function` which abstracts concrete values. A function can
also be called a polymorphic value. The next level of abstraction is an
abstraction of a function itself which is called a `polymorphic function`.

The two most fundamental properties of Haskell that bestow power upon the
programmer are the powerful `abstraction` and `composition` facilities.
`Everything is a value`, be it a function or concrete value and we can compose
values in very flexible and powerful ways.

Concrete Values and Functions
-----------------------------

A `value` is the most general and unifying concept in Haskell. A program is
nothing but definition of values and how to combine them. Ultimately everything
in a Haskell program is a value. There are `concrete values` like strings or
numbers which represent real concrete data. Then there are `abstract values`
which are called functions. A Haskell program can compose abstract values (i.e.
functions) to create new abstract values dynamically but ultimately the
abstract parameters are replaced with concrete arguments to reduce the
abstractions to concrete output values.

The key point is that functions are not special they are just another form of
concrete values or data. Functions and concrete values can be converted back
and forth by the reciprocal processes of `abstraction` and `reduction`.  Since
there is no clear distinction between functions and data they are processed in
similar ways. Hopefully this generalised description will be clearer after
going through the following sections.

::

  Concrete |-------> abstraction    ------>| abstract value
  Value    |<------- reduction or   <------| or function
                 function application

Abstraction of Concrete Values
------------------------------

+-----------------------------------------------------------------------------+
| A `concrete value` is an expression which can be computed to a pure data    |
| without requiring any input (or free variables). Here is a definition       |
| representing a concrete value:                                              |
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
| polymorphic value of arity 1  | ``f1 a     = a  + 10 + 10``                 |
+-------------------------------+---------------------------------------------+
| polymorphic value of arity 2  | ``f2 a b   = a  + b  + 10``                 |
+-------------------------------+---------------------------------------------+
| polymorphic value of arity 3  | ``f3 a b c = a  + b  + c``                  |
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
| result of abstraction.                                                      |
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

Reduction of Abstract values
----------------------------

+-----------------------------------------------------------------------------+
| Reduction is a process which is opposite of abstraction. A `function        |
| application` concretizes or reduces the abstract value represented by a     |
| function by `combining` the function with concrete values corresponding to  |
| the abstracted parameters.                                                  |
+-----------------------------------------------------------------------------+
| When we say `value` in general we mean anything that a function can accept  |
| as its arguments. It could be a `concrete value` or a `function`.           |
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

+-----------------------------------------------------------------------------+
| Function application                                                        |
+========+===+======+=========================================================+
| Output | = | Name | arguments                                               |
+--------+---+------+---------------------------------------------------------+
| r      | = | f3   | v1 v2 v3                                                |
+--------+---+------+---------------------------------------------------------+
| Parameters `a`, `b` and `c` in the function definition get bound to the     |
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
| Everything is a value! `f a` combines an abstract value `f` with the value  |
| `a` to produce a less abstract value.                                       |
+-----------------------------------------------------------------------------+
| This is an asymmetric operation because `f` and `a` have different roles,   |
| which means the operation is not commutative i.e. `f a` is not the same as  |
| `a f`                                                                       |
+-----------------------------------------------------------------------------+
| This operation is left associative i.e. ``f a b c <=> ((f a) b) c``         |
+-----------------------------------------------------------------------------+
| Whitespace as an operator may be clearer if we imagine some other operator  |
| symbol in place of whitespace e.g. ``f @ a @ b @ c``                        |
+-----------------------------------------------------------------------------+

Mathematical Definition of a Function
-------------------------------------

Earlier we described a function as a polymorphic value or an abstract value.
Another way of a looking at a function is as a mapping from the values of input
parameters to the outputs of the function. A function discriminates its inputs
and maps different input values to different output values.

Data & Type level programming
-----------------------------

A Haskell program defines logic to process input data and produce output data.
This logic is defined in terms of functions and function applications. We will
call this part of the program the `data level program`. Along with the data
level program a Haskell program also contains a `type level program` which
ensures the correctness of the data level program at compile time. We will talk
about the basics of a type level program in the next section.

Types: Ensuring Correctness of Data Level Program
-------------------------------------------------

In our (data level) program, how do we make sure that we do not supply
`oranges` as input to a function parameter which only works correctly with
`apples`?

`Every value` (function or data) in the `data level` has a `type` label
associated with it.  Type is a label which identifies a whole class of values
conforming to certain rules or you can say it defines the shape of the data.

The type labels are either explicitly specified by the programmer or determined
automatically by way of inference. At compile time the type level program
annotations are interpreted by the typechecker. The typechecker makes sure that
when we apply a function to an input value the type label of that value matches
the type label of the function input. Therefore if a value is labeled `apple`
the typechecker will refuse to pass the data level program if we feed this
value to a function input which is labeled `orange`.

Value or Function Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
so that distinct types do not get confused by mistake.  The type annotations
for values in data level program can collectively be thought of as a `type
level program`.

The type level program is interpreted at compile time by the `typechecker`.  It
essentially checks if the types used in the data level program are consistent
with the type level program. Some fundamental checks:

* `functions`: The type of the function input must match the type of the value
  being fed to the function.

* `case`: The only way a function maps one type to another is via case
  expression. All the values `mapped from` must have one type and all the
  values `mapped to` must have one type.

* `Equations`: When two values can be substitued in place of each other then
  they must have the same type.

Data Level Program
------------------

Functions & Data
~~~~~~~~~~~~~~~~

A data level program is composed of functions. Functions operate on values.
There are two types of values viz. functions and data. Data is the only
mechanism to transfer values across functions. It is used to represent
inputs and outputs of a program as well as intermediate values passed from one
function to another during computations. Note that data can hold any type of
values, concrete values or even functions (computations). Data is represented
by `algebraic data types` in Haskell.

+-----------------------------------------------------------------------------+
| Values                                                                      |
+---------------------------------------+-------------------------------------+
| Defined Values                        | Constructed Values                  |
+---------------+-----------------------+-------------------------------------+
| Data          | Function Definitions  | Algebraic Data                      |
| Definitions   |                       | Structures                          |
+---------------+-----------------------+-------------------------------------+
| ``v = 10``    | ``f x = x + v``       | ``data Color = Red | Green | Blue`` |
+---------------+-----------------------+-------------------------------------+

Composed Functions
~~~~~~~~~~~~~~~~~~

Composed functions are defined purely in terms of composed applications of
other functions. They pass on their arguments without having to know their
values and hence do not discriminate the logic based on them.  In other words,
they treat their parameters as opaque data.  It means that they do not need to
de-construct the algebraic structure of their arguments.

::

  square x = x * x

This classification is not very interesting as such but it is a concrete
value level equivalent of function-level parametric polymorphism. Such
functions do not discriminate values the way parametrically polymorphic
functions do not discriminate types. We can say that a composed function is a
parametrically polymorphic value.

Higher-order functions
~~~~~~~~~~~~~~~~~~~~~~

A function which takes another function as an argument is a higher order
function. Higher order functions could be of different ranks depending on
whether the function passed as argument also takes another function as argument
and so on.

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

Let us now build a `Shape` data type. A shape could be a triangle or a sqaure.
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
~~~~~~~~~~~~~~~~~~~~~~

The purpose of type level programming is to generate concrete types.
Just like at data level we have `data functions` representing `asbtract` or
`polymorphic data`, the same way at the type level we have `type functions`
representing abstract or `polymorphic types`.  Type functions can be used to
compose types together to create more complex types from simple concrete types.

Note that the type assigned to any data level value is always `concrete`.  The
type of a data value can never be a type function. Type functions only exist at
the type level. See the kinds section for details.

Generating function types
~~~~~~~~~~~~~~~~~~~~~~~~~

What is the type of a function value? A function with one argument is different
from a function with two arguments. A function accepting an `Int` argument is
different from a function accepting `Char` argument. Similarly for return
value. The combinations are huge, so how do we represent so many types uniquely?

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

TBD - deduplicate with the table in the syntax chapter

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

Polymorphic Functions & Types
-----------------------------

Functions whose argument types can vary. They work for many types.

::

  id :: a -> a
  id x = x

The `a` in the signature of this function is a `type variable`. `a` can assume
any concrete type.

`Function instances`: When we apply the `id` function to a value of a
concrete type, then we `instantiate` the type variable `a` to that concrete
type:

::

  id (3 :: Int)

This is also known as `parametric polymorphism`.

Similarly, polymorphic types (type functions) also use type variables::

  data Pair a = Pair a a

Quantification of Type Variables
--------------------------------

A polymorphic function as well as a polymorphic type uses type variables. Like
variables in a data level program, type variables too have scope. The scope of
a type variable is also known as quantification.

Quantification decides the `visibility scope of a type variable` to the
typechecker. The type variable cannot be instantiated and cannot exist outside
that scope. There are two types of quantifications viz.  `univseral` (global
scope) and `existential` (local scope) quantification.

When a type variable is universally quantified it means that the type variable
is valid over the scope of the whole program. The type variable is visible for
typechecking anywhere in the program without any restrictions. Universal
quantification is implicit or default. All type variables of a function are
unviersally quantified by default. Though we can use an explicit `forall`:

::

  id :: forall a. a -> a
  id x = x

Whereas `existential quantification implies that the availability or the scope
of the quantified variable is limited`. The variable cannot exist or typecheck
outside the specified scope. It is represented by a scoped `forall`. For
example:

When we say a type variable is `not quantified`, it means that it is
universally quantified. Whereas just saying `quantified` is equivalent to
saying `existentially quantified`.

TBD: examples of existential quantification.

Data Level Parametric Polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Tips: Understanding a Haskell Program
-------------------------------------

The names or identifiers in one level (data, type or kind) should not be
confused or conflated with the names in other level. An identifier of the same
name can be used in different levels without any problem.

Names of data constructor functions and types could be the same, which can be
confusing for beginners. Similarly type variables in type level and type
parameters in data level could be same or different, they should not be
confused with each other.

Summary
-------

* A function is really the only building block of Haskell
* A Haskell program is a specification of equations for functions
* There are three independent functional programming spaces viz. data, type
  and kind
* The bridge between any two spaces is a function name
