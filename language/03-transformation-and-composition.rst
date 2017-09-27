.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Basic Computing Primitives
==========================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+------------------------+----------------------------------------------------+
| Introduction rule      | When a constructor is used on the RHS to construct,|
|                        | it is called an `introduction rule` in type theory.|
+------------------------+----------------------------------------------------+
| Elimination rule       | When a constructor is used on the LHS to pattern   |
|                        | match or destruct a data structure, it is called   |
|                        | an `elimination rule` in type theory.              |
+------------------------+----------------------------------------------------+
| Type                   | Denotes rules that a value should conform to       |
|                        | (e.g. Int or String)                               |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+

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

Type Checking and Inference
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As long as the programmer assigns unique types to the data structures being
used in a program Haskell guarantees that that distinct types of values cannot
accidentally be used in place of each other.  The types are analyzed at compile
time by the `typechecker`.  It essentially checks if the types used in the
program are consistent and we are not using one type in place of another. Type
checks include:

* `functions`: The type of the function input must match the type of the value
  being fed to the function.

* `Equations`: When two values can be substituted in place of each other then
  they must have the same type.

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

Data Types
----------

Basic Data Types (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

What is Algebraic Data?
~~~~~~~~~~~~~~~~~~~~~~~

Data in Haskell always means algebraic data.  An expression always evaluates to
either a bare function or an Algebraic Data Type.

Algebraic data is constructed using and only using `data constructors` which
are special functions defined as part of data type definitions. Data
constructors create references to data structures on heap. The structure of the
data is defined by the data definition that we will explain shortly.

+---------------------------------------+-------------------------------------+
| Function Definition                   | Algebraic Data Structure            |
|                                       | Definition                          |
+---------------------------------------+-------------------------------------+
| ``f x = x + 10``                      | ``data Color = Red | Green | Blue`` |
+---------------------------------------+-------------------------------------+

Algebraic Data Types
--------------------

Haskell defines a number of primitive data types e.g. `Char`, `Int`, `Word`,
`Float` and `Double`. A `data declaration` creates a new custom type at the
type level and `data constructor` functions to create values of that type at
the term level.  A user defined data type is a structured data type called an
`algebraic data type` and is defined in terms of existing data types.  Data
constructors are functions that create opaque references to the data and `case
analysis` is the only way to de-structure the data and examine its components.

+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
| Data declaration                                                            |
+-------------------------------------+---------------------------------------+
| Term Level                          |  Type Level                           |
+=====================================+=======================================+
| Data constructors (creation)        |                                       |
+-------------------------------------+                                       |
| Case analysis                       |                                       |
| (destructure)                       |  Data Type                            |
+-------------------------------------+---------------------------------------+

Defining
~~~~~~~~

Data constructor function definitions are supplied by the compiler based on the
signatures specified by the programmer through a data declaration. A data
declaration specifies a data type on the LHS and constructor templates on the
RHS.

+---------------------------------------------------------+-----------------------------------------------+
| data declaration (user defined)                         | Available data constructors functions         |
+=========================================================+===============================================+
| data Color = :blue:`Red` | :blue:`Green` | :blue:`Blue` | :blue:`Red` :: Color                          |
|                                                         +-----------------------------------------------+
|                                                         | :blue:`Green` :: Color                        |
|                                                         +-----------------------------------------------+
|                                                         | :blue:`Blue` :: Color                         |
+---------------------------------------------------------+-----------------------------------------------+
| data Triple = :blue:`Triple` Int Int Int                | :blue:`Triple` :: Int -> Int -> Int -> Triple |
+---------------------------------------------------------+-----------------------------------------------+
| Blue color identifiers are data constructor functions that are used at the term level, rest are types.  |
+---------------------------------------------------------+-----------------------------------------------+

GADT syntax is a way of specifying the constructor signatures directly.

Constructing
~~~~~~~~~~~~

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

Sum and Product Types
~~~~~~~~~~~~~~~~~~~~~

+----------------------------+
| Algebraic Data Types (ADT) |
+-------------+--------------+
| Sum         | Product      |
+-------------+--------------+

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

Recursive Types
~~~~~~~~~~~~~~~

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
| The fundamental primitive to realize the mathematical definition of a       |
| function is a `case` expression. A case expression can enumerate all        |
| patterns for an input value and maps them to specified output values.       |
| Case is essentially a type to type map.                                     |
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

Data Declaration
~~~~~~~~~~~~~~~~

+------------------------------------------------------------------------------------------------------+
| A data declaration essentially binds a type in the type space to one or more data constructors in    |
| the data space.                                                                                      |
+------------+-----------------+---+------------------------------+------------------------------------+
| ADT type   | Type Identifier |   | Data Constructor Templates   | Equivalent Constructor Signatures  |
+============+=================+===+==============================+====================================+
| Product    |   data Pair     | = | Pair Int Int                 | Pair  :: Int -> Int -> Pair        |
+------------+-----------------+---+------------------------------+------------------------------------+
| Sum        |   data Count    | = | Red Int | Green Int          | Red   :: Int -> Count              |
|            |                 |   |                              +------------------------------------+
|            |                 |   |                              | Green :: Int -> Count              |
+------------+-----------------+---+------------------------------+------------------------------------+
| Recursive  |   data IntList  | = | Empty | Cons Int IntList     | Empty :: IntList                   |
| (Inductive)|                 |   |                              +------------------------------------+
|            |                 |   |                              | Cons  :: Int -> IntList -> IntList |
+------------+-----------------+---+------------------------------+------------------------------------+

Sum and Product Types
~~~~~~~~~~~~~~~~~~~~~

Data Construction
~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A data constructor is a special function defined by a data declaration, it  |
| creates a reference to an algebraic data type.                              |
+-----------------------------------------------------------------------------+
| x = C a b c ...                                                             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let pair  = Pair 10 20                                                    |
|   let count = Red 5                                                         |
|   let list  = Cons 10 (Cons 20 Empty) :: List Int                           |
+-----------------------------------------------------------------------------+

Pattern Match on a Product Type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| In addition to `case` expression and `function definition` pattern matches  |
| can also be performed in `let` and `where` clauses.                         |
| The same pattern matching rules specified for `case` apply to other         |
| forms as well.                                                              |
+-----------------------------------------------------------------------------+
| Pattern matches in `case` and `function definition` are strict.             |
+-----------------------------------------------------------------------------+
| Pattern matches in `let` and `where` are lazy and irrefutable.              |
+-----------------------------------------------------------------------------+

Deconstructing a Product
^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let pair = Pair 10 20                                                     |
+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case pair of                        |  total (Pair a b) = a + b            |
|    Pair a b -> a + b                 |                                      |
+--------------------------------------+--------------------------------------+
| Let                                  | Where                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  let Pair a b = pair                 |  total = a + b                       |
|  in a + b                            |   where Pair a b = pair              |
+--------------------------------------+--------------------------------------+

Wild Card and Nested Patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Pair = Pair (Int, Int) (Int, Int)                                     |
|  let  pair = Pair (1, 2) (3, 4)                                             |
+-------------------------+---------------------------------------------------+
| Wild card (``_``) match | ``total (Pair _ b)   = b``                        |
+-------------------------+---------------------------------------------------+
| Nested pattern          | ``total (Pair a (i, j))   = i + j``               |
+-------------------------+---------------------------------------------------+
| Nested `As pattern`     | ``total (Pair a b@(i, j)) = (i + j, b)``          |
| (``b`` as ``(i, j)``)   |                                                   |
+-------------------------+---------------------------------------------------+
| `b` is bound to the original argument passed, and `i` and `j` are           |
| bound to the deconstructed components of `b`. Pattern match of `b` is       |
| irrefutable since `b` matches the incoming argument as it is.               |
+-----------------------------------------------------------------------------+

Pattern Match Failure
^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| Patterns that can never fail                                                |
+=============================================================================+
| Wildcards i.e. patterns without data constructors (``_`` or a variable)     |
+-----------------------------------------------------------------------------+
| Pattern match on a single constructor data type.                            |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Refutable patterns                                                          |
+=============================================================================+
| Refutable patterns have alternatives to fall back on, when a refutable      |
| pattern match fails we fall back on the alternative.                        |
| However, if all possible patterns are not captured by all the alternatives  |
| then a runtime error may occur due to non-exhaustive patterns.              |
+-----------------------------------------------------------------------------+
| **Cases**                                                                   |
+-----------------------------------------------------------------------------+
| Patterns in a case analysis                                                 |
+-----------------------------------------------------------------------------+
| Patterns in function parameters, except "as patterns" and lazy patterns     |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Irrefutable patterns                                                        |
+=============================================================================+
| Patterns that are committed for use with no fallback option or alternatives |
| if the pattern match fails.                                                 |
| When an irrefutable pattern match fails it results in a runtime error.      |
+-----------------------------------------------------------------------------+
| **Cases**                                                                   |
+-----------------------------------------------------------------------------+
| Patterns in a top level binding,                                            |
| `let`, and `where`                                                          |
+-----------------------------------------------------------------------------+
| "As patterns"                                                               |
+-----------------------------------------------------------------------------+
| Patterns marked lazy using ``~``                                            |
+-----------------------------------------------------------------------------+

Case Analysis
~~~~~~~~~~~~~

Algebraic data types and case analysis are the primary tools to implement
case-mapped functions.  Case analysis is a mechanism to navigate through the
choices (values) represented by an algebraic data type and map them to outputs.

A `case` expression is the fundamental way (others are syntactic sugars on top
of case) to perform a case analysis by deconstructing an algebraic data type
via `pattern matching` and mapping the individual deconstructions to
corresponding output expressions.

Case Expression
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A `case expression` is a direct translation of the mathematical definition  |
| of a function.                                                              |
| It is a map from individual constructor patterns of an `<input expr>` to    |
| corresponding output expressions.                                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  case <input expr> of                                                       |
|    C1 a b c ... -> <output expr1>                                           |
|    C2 a b c ... -> <output expr2>                                           |
|    x            -> <output expr3>                                           |
|    ...                                                                      |
+-----------------------------------------------------------------------------+
| `<input expr>` is called the `scrutinee` of the case expression.            |
+-----------------------------------------------------------------------------+
| Each line under the case statement specifies a mapping, from a constructor  |
| pattern - matching the scrutinee - to an output expression.                 |
+-----------------------------------------------------------------------------+
| C1, C2 etc. are the constructors defined by the type of `<input expr>`.     |
+-----------------------------------------------------------------------------+
| ``a`` ``b`` ``c`` are variables corresponding to the components of the      |
| product type (if any) represented by the chosen constructor.                |
+-----------------------------------------------------------------------------+
| Patterns are matched from top to bottom. First pattern that matches the     |
| constructor of the scrutinee is chosen and the corresponding output         |
| expression is evaluated.                                                    |
+-----------------------------------------------------------------------------+
| This process of selecting a matching constructor of the sum type and then   |
| breaking apart the components of a product type constructor is called a     |
| `pattern match`.                                                            |
+-----------------------------------------------------------------------------+
| Patterns can be nested i.e. ``a`` ``b`` ``c`` themselves can be specified   |
| patterns deconstructing them further.                                       |
+-----------------------------------------------------------------------------+
| If the pattern being matched is a variable (e.g. ``x``) or ``_`` the match  |
| will always succeed (irrefutable). In case of ``_`` the input is discarded  |
| while in case of a variable the input is bound to that variable.            |
+-----------------------------------------------------------------------------+
| The output expressions can make use of the bindings ``a``, ``b``, ``c``.    |
+-----------------------------------------------------------------------------+
| All the output expressions must be of the same type i.e. the result type of |
| the case expression.                                                        |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Some important facts about `case` and `pattern match`                       |
+=============================================================================+
| Case is the fundamental way to pattern match in Haskell. All other forms of |
| pattern matches are just syntactic sugar on top of case. It is helpful to   |
| think of other forms of pattern matches in terms of case to better          |
| understand them.                                                            |
+-----------------------------------------------------------------------------+
| The `scrutinee` of case is strictly evaluated to WHNF to enable the pattern |
| match. This is the exclusive source of all forms of strict evaluation in    |
| Haskell.                                                                    |
+-----------------------------------------------------------------------------+
| If you think about it, the fundamental purpose of branching in a            |
| programming language is to create a mapping - a function in mathematical    |
| sense. In Haskell, a case expression represents a function more explicitly; |
| therefore it does not have a separate branching primitive. All forms of     |
| branching is just syntactic sugar on top of case.                           |
+-----------------------------------------------------------------------------+

Selecting Alternatives of a Sum
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  let count = Red 5                                                          |
+-----------------------------------------------------------------------------+

+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case count of                       |  name Red   i = "R " ++ show i       |
|    Red   i -> "R " ++ show i         |  name Green i = "G " ++ show i       |
|    Green i -> "G " ++ show i         |                                      |
+--------------------------------------+--------------------------------------+
| Pattern match on sum type may fail at run time with a `non-exhaustive       |
| pattern match` error if it does not cover all constructors.                 |
+-----------------------------------------------------------------------------+
| Patterns are matched from top to bottom in sequence.                        |
+-----------------------------------------------------------------------------+

+--------------------------------------+--------------------------------------+
| Let                                  | Where                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  let Red i = count                   |  reds = "R " ++ show i               |
|  in "R " ++ show i                   |    where Red i = count               |
|                                      |                                      |
|  -- this match will fail             |  -- this match will fail             |
|  let Green i = count                 |  greens = "G " ++ show i             |
|  in "G " ++ show i                   |    where Green i = count             |
+--------------------------------------+--------------------------------------+
| Pattern matches in `let` and `where` are lazy or irrefutable. We can match  |
| any or all constructors but it may fail when we use the value belonging to  |
| a non-matching constructor.                                                 |
+-----------------------------------------------------------------------------+

Case: Extended Syntax
^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| -XLambdaCase                                                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  \x -> case x of                     |  \case                               |
|    ...                               |      ...                             |
+--------------------------------------+--------------------------------------+

+-----------------------------------------------------------------------------+
| -XEmptyCase                                                                 |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case e of { }                       |  \case { }                           |
+--------------------------------------+--------------------------------------+

Transformation and Composition
------------------------------

Transforming one type into another and composing multiple objects of different
types together are two fundamental computing operations. Any logic program can
be implemented using these two fundamental primitives.

Algebraic data constructors are the essence of composition and case expression
is the essence of transformation.

+---------------------------------+-------------------------------------------+
| Transformation                  | Case Analysis                             |
+---------------------------------+-------------------------------------------+
| Composition                     | Constructors                              |
+---------------------------------+-------------------------------------------+

Transformation
~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Transformation is a unary operation that maps one type to another.          |
| The type being mapped from can potentially be a product type.               |
+===================================+=========================================+
| Input (Consume)                   | Output (Produce)                        |
+-----------------------------------+-----------------------------------------+
| The fundamental instrument of transformation is a case expression.          |
| Transformation starts with destruction of the source type and proceeds with |
| construction of the destination type.                                       |
+-----------------------------------------------------------------------------+

Case is the only fundamental construct involving two different types, an input
type and an output type, mapping the input to the output.  It destructures the
input type using pattern match on its constructors and then constructs the
output type using its constructors.  Therefore, all the output expressions in
the following table must have the same type which is the output type of the
case expression.

+-----------------------------------------------------------------------------+
| Case type checking                                                          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  case <input expr> of                                                       |
|    C1 a b c ... -> <output expr1>                                           |
|    C2 a b c ... -> <output expr2>                                           |
|    x            -> <output expr3>                                           |
|    ...                                                                      |
+-----------------------------------------------------------------------------+

Case is the essence of a mathematical definition of a function. All other
abstractions including functions, boolean operations, branching etc. are built
on top of case and algebraic data constructors.

Composition
~~~~~~~~~~~

Composition is a general way of combining multiple objects of potentially
different types into a single object. Pure basic composition is just putting
types together as a product type using a constructor. Later we will discuss
higher level abstractions like functions and more that compose and transform
types in interesting ways.

+-----------------------------------------------------------------------------+
| Composes a finite number (not a stream) of  objects of potentially          |
| different types.                                                            |
+================+============================================================+
| N-ary          | A constructor just stores multiple data types together as  |
| constructor    | a product type.                                            |
|                +------------------------------------------------------------+
|                | ``C :: A -> B -> C``                                       |
+----------------+------------------------------------------------------------+

Basic Algebraic Data Types (Prelude)
------------------------------------

* Lists should be introduced after we explain recursion
* TODO: provide links to the definitions in base
* Provide the definitions as well

+----------+----------------------------------+-------------------------------+
| Type     | Values                           | Description                   |
+==========+==========+==========+============+===============================+
| Bool     | True     | False    |            |                               |
+----------+----------+----------+------------+-------------------------------+
| [a]      | []       | 1 : []   | 1 : 2 : [] | List of Int                   |
|          |          |          |            | Explicit constructor syntax   |
|          +----------+----------+------------+-------------------------------+
|          | []       | [1]      | [1,2]      | Sugared syntax                |
|          +----------+----------+------------+-------------------------------+
|          | []       | ['a']    | ['a','b']  | List of chars (String)        |
|          +----------+----------+------------+-------------------------------+
|          | ""       | "a"      | "ab"       | String literals               |
+----------+----------+----------+------------+-------------------------------+
| ()       | ()       |          |            | Unit data type, empty tuple   |
+----------+----------+----------+------------+-------------------------------+
| (a, b)   | (1, 'a') | (0.3, 1) | (1, 2)     | Two Tuple                     |
+----------+----------+----------+------------+-------------------------------+
| Ordering |  LT      | EQ       | GT         |                               |
+----------+----------+----------+------------+-------------------------------+

Bool
~~~~

Comparisons resulting in Booleans (Prelude)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------+-------------+-------------------------+
| ==        | 3 == 2      |  Equals                 |
+-----------+-------------+-------------------------+
| /=        | 3 /= 2      |  Not equal              |
+-----------+-------------+-------------------------+
| >         | 3 >  2      |  Greater than           |
+-----------+-------------+-------------------------+
| >=        | 3 >= 2      |  Greater than or equal  |
+-----------+-------------+-------------------------+
| <         | 3 <  2      |  Less than              |
+-----------+-------------+-------------------------+
| <=        | 3 <= 2      |  Less than or equal     |
+-----------+-------------+-------------------------+

Operations on Booleans (Prelude)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------+---------------+-------------------------+
| Operation | Example       | Remarks                 |
+===========+===============+=========================+
| ==        | True == False |                         |
+-----------+---------------+-------------------------+
| /=        | True /= False |                         |
+-----------+---------------+-------------------------+
| ||        | True || False |                         |
+-----------+---------------+-------------------------+
| &&        | True && False |                         |
+-----------+---------------+-------------------------+
| not       | not True      |                         |
+-----------+---------------+-------------------------+

Branching on Booleans
^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| `if` statement is just a syntactic sugar on top of a `case` scrutiny on     |
| `Bool`                                                                      |
+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  case predicate of                 |  if predicate                          |
|    True ->  expr1                  |  then expr1                            |
|    False -> expr2                  |  else expr2                            |
+------------------------------------+----------------------------------------+

+-----------------------------------------------------------------------------+
| Boolean Guards                                                              |
+-----------------------------------------------------------------------------+
| A pattern match selects a branch solely based on the constructor            |
| pattern. However, it can always be refined by adding boolean `guards`.      |
+-----------------------------------------------------------------------------+
| * Guards are specified as comma separated boolean conditions.               |
| * Guards can use deconstructed variables in conditions.                     |
| * If a condition results in ``False`` the guard and the pattern match fails.|
+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case count of                       |  name Red   i | i < 5 = "R few"      |
|    Red   i | i < 5                   |  name Red   i | i >= 5, i < 10       |
|            -> "R few"                |                       = "R some"     |
|    Red   i | i >= 5, i < 10          |  name Red   _         = "R many"     |
|            -> "R some"               |  name Green i = "G " ++ show i       |
|    Red _   -> "R many"               |                                      |
|    Green i -> "G " ++ show i         |                                      |
+--------------------------------------+--------------------------------------+

+-----------------------------------------------------------------------------+
| Multi-way conditions using guards                                           |
+--------------------------------------+--------------------------------------+
| Using case on `()` and guards        | Using `-XMultiWayIf`                 |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case () of                          |  if | guard1 -> expr1                |
|    _ | guard1 -> expr1               |     | ...                            |
|    ...                               |     | guardN -> exprN                |
|    _ | guardN -> exprN               |                                      |
+--------------------------------------+--------------------------------------+
| You can have nested multiway-conditions too.                                |
+-----------------------------------------------------------------------------+

Lists
~~~~~

::

  data []   a = []    | :    a (List a)                -- Recursive

Note that Haskell's built-in list is not really a special syntax it is a user
defined data type, '[]' is the empty list constructor and ':' is the Cons
constructor. Though there is a syntactic sugar to specify lists in a more
convenient way [1, 2] is equivalent to 1 : 2 : [].

* List comprehensions
* See prelude for list functions

Tuples
~~~~~~

* TBD
* TBD - tuple sections


Type Signatures
~~~~~~~~~~~~~~~

Ideally the only place where a programmer needs to provide types is a data type
declaration. The whole program then infers the types with the data types taken
as the anchors. However, there may be situations where the inferred type may be
ambiguous. In such cases, the programmer can provide type annotations or type
signatures to remove the ambiguity. Also, it is recommended to specify type
signatures for all top level declarations it helps in diagnosing the type
errors. One way to narrow down type errors is by specifying type signatures to
the known types involved in an expression.

A programmer can specify type signatures at the following places:

* declarations - function definitions, let or where clauses
* expressions - any part of an expression can be given a type
* pattern matches

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

Type Level Syntax
-----------------

Type Signatures
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A type signature can be associated with an identifer or an expression using |
| the ``::`` operator which can be read as `has type`.                        |
+----------------+------------------------------------------------------------+
| Type signature | ``<identifier or expression> :: <type>``                   |
+----------------+------------------------------------------------------------+
| A type is denoted by an identifier, or an expression involving type         |
| functions. Type level identifiers live in their own namespace.              |
+-----------------------------------------------------------------------------+

+--------------------+--------------------------------------------------------+
| Identifier         | ::                                                     |
|                    |                                                        |
|                    |   v :: Int                                             |
|                    |   v = 10                                               |
+--------------------+--------------------------------------------------------+
| Expression         | ::                                                     |
|                    |                                                        |
|                    |   v = 10 :: Int                                        |
+--------------------+--------------------------------------------------------+
| Typed Holes (GHC 7.8.1)                                                     |
+-----------------------------------------------------------------------------+
| Use ``_`` wildcard in place of a value to indicate a type hole. GHC         |
| will report the inferred type of the value to be used in place of the hole. |
+--------------------+--------------------------------------------------------+
| Typed hole         | ::                                                     |
|                    |                                                        |
|                    |  v :: Int                                              |
|                    |  v = _ + 10                                            |
+--------------------+--------------------------------------------------------+

Plugs and Sockets
~~~~~~~~~~~~~~~~~

If a value expression is a plug and the function input is a socket, the type
checker makes sure that the plugs correctly fit into the sockets. Haskell
program is a network of different types of plugs and sockets.

`Inference`: If two plugs fit into the same socket then they must be of the
same type. If two sockets accept the same plug then they must be of the same
type.

Insert graphic plug and socket.
Insert "input >=> output" Haskell program zigsaw puzzle.

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

Type & Data Namespaces
----------------------

Type and data identifiers have their own distinct namespaces. Types (e.g. Int)
always start with an uppercase letter, however type level variables start with
a lowercase letter. Everything in data namespace except data constructors,
which are discussed later, start with a lowercase letter. Data constructors
always start with an uppercase letter.

+-----------------------------------------------------------------------------+
| Identifiers starting with a `lowercase` letter                              |
+------------------------------------+----------------------------------------+
| type variables (type namespace)    | term variables (data namespace)        |
+------------------------------------+----------------------------------------+
| These two namespaces can use the same identifier name without conflict.     |
| The compiler can distinguish them by the context.                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- The following is a valid Haskell code where the identifier 'play'       |
|  -- refers to multiple distinct objects in two independent namespaces       |
|  play ::            -- 'play' refers to a function name in data namespace   |
|       play -> play  -- 'play' is a type variable in type namespace          |
|  play play = ...    -- both 'play' are term variables in data namespace     |
|                     -- first one refers to the name of the function name    |
|                     -- and second one to a parameter of the function        |
+-----------------------------------------------------------------------------+

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

References
----------

https://en.wikipedia.org/wiki/Pattern_matching
https://en.wikipedia.org/wiki/Proof_by_exhaustion case analysis

