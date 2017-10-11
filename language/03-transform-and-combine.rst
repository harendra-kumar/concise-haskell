.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Composition: Transform and Combine
==================================

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+------------------------+----------------------------------------------------+
| Type                   | Denotes rules that a value should conform to       |
|                        | (e.g. Int or String)                               |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+

Overview
--------

Haskell is all about composing programs and we will learn in this chapter that
the essential tools for composing are transformation and combining. These two
basic concepts will run as a recurring theme everywhere in this text in
different forms and at different levels of abstraction.

Turing Machine: Logic and Data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Logic and Data:` For performing a computation we need memory and processing
logic. In a Haskell program algebraic data is the memory, and functions are the
logic. Functions encode the decision making or the branching logic as a map
from one type to another. A function retrieves data from an algebraic data
structure, processes it and stores the results back into another algebraic data
structure.

.. In fact, functions can also be represented as data, see representable
   functors.  But we will talk about functions as functions for simplicity.

`Transform and Combine:` All expressions in Haskell evaluate to either a bare
function or an Algebraic Data Type.  There are two basic operations that we can
perform on functions and data, (1) we can combine two or more data types into a
new data type, (2) we can transform a data type into another data type using
some logic. All Haskell programs can be imagined as a combination of these two
basic operations.

Data Types
----------

Types of Terms and Expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A common programming mistake in `untyped` or weakly typed languages is using a
wrong value i.e. use an `orange` in a computation where we were supposed to use
an `apple`. How do we avoid such mistakes and ensure the correctness of a
program?

Haskell expressions are made of functions applied to terms. Every term in an
expression has a unique `type` associated with it.  A type is a label that
determines the legal values that the data can assume.  A function application
in an expression is the `only way` to combine terms and produce new terms.
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

.. Use shapes e.g. triangle and square rather than apple and oranges. Shapes
   can be used as a recurring theme for comparing types.

Types can usually be inferred via `type inference` but if there is an ambiguity
the programmer can explicitly specify the type of an expression or function
using `type signatures` (also known as `type annotations`).

.. Add examples and exercises

Type Checking and Inference
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As long as the programmer assigns unique types to the data structures being
used in a program, Haskell guarantees that distinct types of values cannot
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

.. Add an example of inference

Algebraic Data Types
--------------------

Built-in Algebraic Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Defining Algebraic Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Algebraic data is constructed using and only using `data constructors` which
are special functions defined as part of data type definitions. Data
constructors create references to data structures on heap. The structure of the
data is defined by the data definition that we will explain shortly.

Haskell defines a number of built-in data types e.g. `Char`, `Int`, `Word`,
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

Case Analysis
~~~~~~~~~~~~~

A case analysis allows us to examine sum or product types. A sum represents
multiple choices whereas a product represents a set or collection. As we keep
combining types with sum or product we keep forming a tree in which the choices
are the branches and sets are the nodes.

In a case construct a pattern deconstructs a set into its components while the
options of the case selects the branch corresponding to a choice. The
combination of the two allows us to navigate any part of the ADT tree.

It allows us to navigate through and pick any value represented by the
type and map it to another value. Or map a certain set of values in the same
way and another set in a different way by matching the sets.

data Polygons = Triangles | Squares | Pentagons -- 3 values
data Colors = Red | Green | Blue -- 3 values
data Sizes = Big | Small | Tiny

data ColoredPoly = ColoredPoly Polygons Colors -- 9 values
data SizedPoly = SizedPoly Polygons Sizes -- 9 values

data AllPoly = ColoredPoly | SizedPoly -- 18 values

data  PolyUniverse = PolyU Polygons Colors Sizes -- 3x3x3 = 27 values

case poly of
  ColoredPoly -> case ColoredPoly of
                    ColoredPoly p c -> case p of
                                          Triangle -> case c of
                                                        Red -> "red triangle"
                                                        Green ->
                                                        Blue ->
                                          Square   ->
                                          Pentagon ->
  SizedPoly ->

  data Alpha = A | B | C
  data Num = One | Two | Three

  data AlphaNum = AlphaNum Alpha Num

Create a picture of the tree of all choices
  - first of just polygons
  - then make it coloredpoly
  - then make it allpoly

Algebraic Data Types
~~~~~~~~~~~~~~~~~~~~

A type represents a number of choices or values. For example, an `Int` type
represents 2^64 choices on a 64 bit machine each representing a different
number. An RGB type may represent one of three colors `Red`, `Green` and
`Blue`.  A binary digit type may represent either `Zero` or `One`. In Haskell
it can be represented as:

::

  data Bit = Zero | One

This is a primitive algebraic data type. A primitive algebraic data type is one
which is not defined in terms of any other algebraic data types.  `Bit` is a
`sum` type as the total number of choices are the sum of individual choices (1
+ 1). A primitive algebraic data type is always a sum type since we have to
enumerate all the choices represented by the data type.

We can build more complex algebraic data types by defining a new type as a
`sum` or `product` of existing types. For example a 2-bit word can be defined
as a product of two `Bit` types:

::

  data Word2 = Word2 Bit Bit

Here we are saying that a `Word2` is a set of 2 `Bit` s. Since each `Bit`
is a sum type having two possibilities `Zero` or `One`, the type `Word2`
has 4 distinct combinations:

+-----------+----+
| Zero Zero | 00 |
+-----------+----+
| Zero One  | 01 |
+-----------+----+
| One Zero  | 10 |
+-----------+----+
| One One   | 11 |
+-----------+----+

The data type `Word2` therefore represents a total of 4 choices or 4 values.
The total choices represented by Word2 is a product of the choices represented
by each Bit type i.e. 2x2. That's why it is called a product type. A Word2
therefore is a product of two sum types.

Let us now build a `Shapes` data type which describes shapes and for each shape
it also describes its color and how many of them are there.  Our shapes could
either be a triangle or a square.

::

  data Colors = Red | Green | Blue                            -- 3 values
  data Shapes = Triangles Color Word2 | Squares Color Word2   -- 3x4 + 3x4 = 24

This data type is a sum of products which is built using a `Color` data type
and a `Word2` data type. The Shapes data type describes a total of 24 values.

+----------------------------+
| Algebraic Data Types (ADT) |
+-----+---------+------------+
| Sum | Product | Recursive  |
+-----+---------+------------+
| Data constructors          |
+----------------------------+

As a function is the fundamental building block in Haskell, even user
defined data is represented by a function called a `data constructor`.

There are two fundamental data types:
- Put multiple things together
- One of many choices

Recursive data type - e.g. linked list - product type involving itself

A data constructor puts together
representation is to call
a function using existing data representations as arguments.

We pattern match on that sum type. The pattern match will enumerate all
possibilities and match with the one which this particular instance represents.
We always have to match against all possiblities in all cases of this data
type.

Sum as the fundamental type
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A type is nothing but a collection of choices.  A sum type is an enumeration of
values belonging to the type.  Using a sum type we can assign different tags to
different choices of a type.

::

  data Void            -- No choices, cannot be constructed
                       -- can be used only at type level, not term level
  data A = A           -- Single choice, tagged A
  data Bool = True | False -- Two choices, tagged True, False (sum)

In addition to enumerating choices a sum type can also put all the choices of
two types together in one type (coproduct). The choices from each type can be
tagged differently to uniquely identify all choices in the combined type. The
total number of choices are a sum of the choices of all component types.

Choices have a correspondence with the transformation operation. If there are
no choices there is nothing to transform no choices to map from or map to i.e.
there is no need to branch.  Having choices is equivalent to having the ability
to map. So choices are an integral part of the transform operation.

Sum is the most fundamental type. There is an important difference between sum
and product types indicating an asymmetry between the two. A product type
always builds types from existing types whereas a sum type can build new
primitive types.  Just like addition is a primitive operation and
multiplication is just a convenient tool to do repeated addition.

We must note that a Sum can combine things of different types into a single
overarching type, creating a container of varied things, each requiring an
ad-hoc or case analysis based handling. Whereas a product combines multiple
things of exactly the same type. Therefore a product type allows for pattern
based handling, all the elements of a product can be handled in the same way.
In general, a product is an expression of a pattern whereas a sum is an
enumeration or a collection of different things.

A product of sums can be expressed as a sum of products and vice-versa?

Product as a multiplier of choices
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A product type is a nested combining rather than a flat combining like sum, it
multiplies all the choices from one type with all the choices from another
type. That is, for each choice from type 1 we have a choice to make from type
2. In imperative terms this is like nested for loops, if we have to enumerate
all the choices in the type we can write something like "foreach type1
{foreach type2 {print (type1, type2)}}"

::

  data A = A X Y       -- X x Y choices, tagged A    (product)
  Either X Y           -- X + Y choices (coproduct)

Types as Shapes
~~~~~~~~~~~~~~~

When we want to think about types in more concrete terms we often refer to them
as shapes. A function input hole expects objects of the shape matching the
shape of the hole. Objects of the same shape can be stacked on top of each
other to form a product but objects of different shapes cannot be. Objects of
any shape can be put together as a sum to form a composite shape.

When multiple choices form a pattern we have a product type. For example
`Product A B`, this type defines many objects, all of them are of the same
shape, the shape includes one part from A and another from B. On the other hand
in a sum type all the choices combine together to form one composite whole e.g.
`Sum A B`, this type defines one composite object that includes all the choices
from A and all the choices from B.

.. relate sum and products with counting. How multiplication is a pattern.
   extend this to multidimensional things. Only things of similar shape can be
   multiplied.

.. A car is a sum of its constituent types. There are many different types of
   components that joined together make a car. A stack of tyres is a product.
   Only similar things can be stacked together. Show a picture of slotted
   shapes of the same type that fit together and can be stacked. A
   representable functor is like that each component having similar shape so
   that it can be represented as a product.

`Fixed vs Variable Shape Sum Types:` All the objects belonging to a product
type always have the same shape by construction. However, the objects or
choices of a Sum type may be of the same shape or they may have different
shapes. When all the tags of a sum type have the same shape we call it a fixed
shape type otherwise a variable shape type.

When a type has a fixed shape a single function can operate on all the choices
of the type without requiring a case analysis. A variable shape type
necessarily requires a case analysis to discriminate between different types of
shapes and handle them accordingly. A type having all the choices of the same
shape is essentially equivalent to a product type.

Coproducts
~~~~~~~~~~

* Note a product of types can be defined in terms of other types.
* A sum type in Haskell  is a sum of data constructor tags belonging to the
  same type and not a sum of other types. A sum of other types would be called
  a coproduct.
* However the only way to define a sum type in terms of other types (i.e. a
  coproduct of types) is Either. Either is a product type that represents a sum
  of two types!
* dependent-sum package generalizes either
* dependent-map?

Case Analysis
-------------

Deconstructing Data By Pattern Matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Transform and Combine
---------------------

Logic and Data:

Transforming one type into another and combining multiple objects of different
types together are two fundamental computing operations. Any logic program can
be implemented using these two fundamental primitives.

Algebraic data constructors are the essence of combining and case expression
is the essence of transformation.

+---------------------------------+-------------------------------------------+
| Transform                       | Case Analysis                             |
+---------------------------------+-------------------------------------------+
| Combine                         | Constructors                              |
+---------------------------------+-------------------------------------------+

Mathematical function
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| TODO: explain how the parameters are mapped to a value by the function      |
| red -> red ball, blue -> blue ball, green -> green ball etc.                |
| Or when one parameter is applied to a three param function how it maps to   |
| a function of two params and so on.                                         |
+-----------------------------------------------------------------------------+

A picture here with input data -> case pattern match (this is basically a
function) -> function application -> output data.

+-----------------------------------------------------------------------------+
| Case expressions: Map input values to output values                         |
+=============================+=================+=============================+
| Decompose and inspect input | Decision switch | Compose output              |
+-----------------------------+-----------------+-----------------------------+
| Pattern Match               | case            | Function Application        |
+-----------------------------+-----------------+-----------------------------+

Transform
~~~~~~~~~

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
type and an output type, mapping the input to the output.  That's the `only`
way to transform types. It destructures the input type using pattern match on
its constructors and then constructs the output type using its constructors.
Therefore, all the output expressions in the following table must have the same
type which is the output type of the case expression.

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

Combine
~~~~~~~

Combining is a process of joining multiple objects of potentially different
types into a single object of another type. Pure basic combining is just
putting types together as a product type using a constructor. Later we will
discuss higher level abstractions like functions and more that compose and
transform types in interesting ways.

+-----------------------------------------------------------------------------+
| Combines a finite number (not a stream) of  objects of potentially          |
| different types into another type.                                          |
+================+============================================================+
| N-ary          | A constructor just stores multiple data types together as  |
| constructor    | a product type.                                            |
|                +------------------------------------------------------------+
|                | ``C :: A -> B -> C``                                       |
+----------------+------------------------------------------------------------+

Syntax In Types
---------------

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

Pattern Matching
~~~~~~~~~~~~~~~~

Refer to the `Basic Syntax` chapter for basic pattern matching.

+-----------------------------------------------------------------------------+
| A lazy pattern match does not force evaluation of the scrutinee.            |
| For example `f undefined` will work on the following:                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   f ~(x,y) = 1    -- will not evaluate the tuple                            |
+-----------------------------------------------------------------------------+
| Since it does not evaluate the scrutinee it always matches i.e. it is       |
| irrefutable. Therefore any patterns after a lazy pattern will always be     |
| ignored. For this reason, lazy patterns work well only for single           |
| constructor types e.g. tuples.                                              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f ~(Just x) = 1                                                            |
|  f Nothing   = 2    -- will never match                                     |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| -XBangPatterns: make pattern matching strict by prefixing it with a ``!``   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f1 !x = True       -- it will always evaluate x                            |
|  f2 (!x, y) = [x,y] -- nested pattern, x will always get evaluated          |
+-----------------------------------------------------------------------------+
| TODO more on bangpatterns, -XStrictData, -XStrict,                          |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| -XPatternGuards: deconstruct a value inside a guard                         |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- boolean guards can be freely mixed with pattern guards                  |
|  f x | [(y,z)] <- x                                                         |
|      , y > 3                                                                |
|      , Just i <- z                                                          |
|      = i                                                                    |
+-----------------------------------------------------------------------------+
| Inside a guard expression, pattern guard ``<pat> <- <exp>`` evaluates       |
| ``<exp>`` and then matches it against the pattern ``<pat>``:                |
|                                                                             |
| * If the match fails then the whole guard fails                             |
| * If it succeeds, then the next condition in the guard is evaluated         |
| * The variables bound by the pattern guard scope over all the remaining     |
|   guard conditions, and over the RHS of the guard equation.                 |
+-----------------------------------------------------------------------------+
| -XViewPatterns: Pattern match after applying an expression to the incoming  |
| value                                                                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  example :: Maybe ((String -> Integer,Integer), String) -> Bool             |
|  example Just ((f,_), f -> 4) = True -- left match can be used on right     |
|                                                                             |
|  example :: (String -> Integer) -> String -> Bool                           |
|  example f (f -> 4) = True           -- left args can be used on right      |
+-----------------------------------------------------------------------------+
| Inside any pattern match, a view pattern ``<exp> -> <pat>`` applies         |
| ``<exp>`` to whatever we’re trying to match against, and then match the     |
| result of that application against ``<pat>``:                               |
|                                                                             |
| * In a single pattern, variables bound by patterns to the left of a view    |
|   pattern expression are in scope.                                          |
| * In function definitions, variables bound by matching earlier curried      |
|   arguments may be used in view pattern expressions in later arguments      |
| * In mutually recursive bindings, such as let, where, or the top level,     |
|   view patterns in one declaration may not mention variables bound by other |
|   declarations.                                                             |
| * If ⟨exp⟩ has type ⟨T1⟩ -> ⟨T2⟩ and ⟨pat⟩ matches a ⟨T2⟩, then the whole   |
|   view pattern matches a ⟨T1⟩.                                              |
+-----------------------------------------------------------------------------+
| -XNPlusKPatterns                                                            |
+-----------------------------------------------------------------------------+
|  TBD                                                                        |
+-----------------------------------------------------------------------------+

Useless pattern matches
^^^^^^^^^^^^^^^^^^^^^^^

When a pattern match does not a bind a variable, it is useless.

::

  x = 2
  y = Just 5

  -- pattern matches without producing a binding:
  1 = 2
  1 = x

  Nothing = Just 5
  Nothing = y

Though if you make the match strict it can be used as an assert::

  -- these will fail at runtime
  let !1 = 2 in "hello"
  let !Nothing = y in "hello"

Pattern Synonyms
~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `-XPatternSynonyms` (7.8.1)                                                 |
+=============================================================================+
| A pattern synonym is a function that generates a pattern or a constructor   |
+---------------------+-------------------------------------------------------+
| Match only          | ::                                                    |
|                     |                                                       |
|                     |  -- match the head of a list                          |
|                     |                                                       |
|                     |  pattern HeadP x <- x : xs  -- define                 |
|                     |  let HeadP x = [1..]        -- match                  |
+---------------------+-------------------------------------------------------+
| Match and construct or `bidirectional` pattern synonyms:                    |
|                                                                             |
| * all the variables on the right-hand side must also occur on the left-hand |
|   side                                                                      |
| * wildcard patterns and view patterns are not allowed                       |
+---------------------+-------------------------------------------------------+
| Match and construct | ::                                                    |
| (Symmetric)         |                                                       |
|                     |  -- match or construct a singleton list               |
|                     |  pattern Singleton x  =  [x]  -- define               |
|                     |                                                       |
|                     |  let single = Singleton 'a'   -- construct            |
|                     |  let Singleton x = [1]        -- match                |
+---------------------+-------------------------------------------------------+
| Match and construct | ::                                                    |
| (Asymmetric)        |                                                       |
|                     |  pattern Head x <- x:xs where   -- define match       |
|                     |      Head x = [x]               -- define construct   |
|                     |                                                       |
|                     |  let list = Head 'a'            -- construct          |
|                     |  let Head x = [1..]             -- match              |
+---------------------+-------------------------------------------------------+
| * Bidirectional patterns can be used as expressions                         |
| * You can use view patterns in pattern synonyms                             |
+---------------------+-------------------------------------------------------+
| A pattern synonym:                                                          |
|                                                                             |
| * starts with an uppercase letter just like a constructor.                  |
| * can be defined only at top level and not as a local definition.           |
| * can be defined as infix as well.                                          |
| * can be used in another pattern synonym or recursively                     |
+-----------------------------------------------------------------------------+
| Import and export                                                           |
+-----------------------------------------------------------------------------+
| Standalone                                                                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  module M (pattern Head) where ... -- export, only the pattern              |
|  import M (pattern Head)           -- import, only the pattern              |
|  import Data.Maybe (pattern Just)  -- import, only data constructor 'Just'  |
|                                    -- but not the type constructor 'Maybe'  |
+-----------------------------------------------------------------------------+
| Bundled with type constructor                                               |
| (must be same type as the type constructor)                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  module M (List(Head)) where ...     -- bundle with List type constructor   |
|  module M (List(.., Head)) where ... -- append to all currently bundled     |
|                                      -- constructors                        |
+-----------------------------------------------------------------------------+
| Expressing the types of pattern synonyms                                    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- General type signature                                                  |
|  pattern P ::                                                               |
|            CReq                 -- constraint required to match the pattern |
|         => CProv                -- constraint provided on pattern match     |
|         => t1 -> t2 -> ...      -- parameters                               |
|  pattern P var1  var2  ... <- pat                                           |
|                                                                             |
|  -- Type signature with CProv omitted                                       |
|  pattern P :: CReq => ...                                                   |
|                                                                             |
|  -- Type signature with Creq omitted                                        |
|  pattern P :: () => CProv => ...                                            |
|                                                                             |
|  -- When using a bidirectional pattern synonym as an expression,            |
|  -- it has the following type:                                              |
|  (CReq, CProv) => t1 -> t2 -> ...                                           |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| A record pattern synonym behaves just like a record.                        |
| (Does not seem to work before 8.0.1)                                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  pattern Point :: Int -> Int -> (Int, Int)                                  |
|  pattern Point {x, y} = (x, y)                                              |
+-----------------------------------------------------------------------------+
| All record operations can be used on this definition now.                   |
+-----------------------------------------------------------------------------+
| A pattern match only record pattern synonym defines record selectors as well|
+---------------+---------------------------+---------------------------------+
| Construction  | ``zero = Point 0 0``      | ``zero = Point { x = 0, y = 0}``|
+---------------+---------------------------+---------------------------------+
| Pattern match | ``f (Point 0 0) = True``  | ``f (Point { x = 0, y = 0 })``  |
+---------------+---------------------------+---------------------------------+
| Access        | ``x (0,0) == 0``                                            |
+---------------+-------------------------------------------------------------+
| Update        | ``(0, 0) { x = 1 } == (1,0)``                               |
+---------------+-------------------------------------------------------------+

Pattern Synonyms Notes
~~~~~~~~~~~~~~~~~~~~~~

Give name to unstructured data:

We can use pattern synonyms to give a name to otherwise unidentifiable data
values. For example, if we have to pattern match on certain integers::

  f 1 = ...
  f 2 = ...
  f 3 = ...

Instead we can use::

  pattern One <- 1
  pattern Two <- 2
  pattern Three <- 3

  f One = ...
  f Two = ...

The alternative would be::

  data MyNums = One Int | Two Int | Three Int
  toMyNums 1 = One 1
  toMyNums 2 = Two 2

  fromMyNums One = 1

But this has a runtime cost.

* https://ocharles.org.uk/blog/posts/2014-12-03-pattern-synonyms.html
* https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms
* https://mpickering.github.io/posts/2014-11-27-pain-free.html

Operational Aspects of Pattern Matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a data element, a pattern match essentially identifies the individual
constructor if it is a sum type and then branches to a target code based on the
constructor. The target code can then break it down into its components if it
is a product constructor.

A data element of a given type is physically represented by a closure on heap.
When a type has 8 or fewer constructors the lowest three bits of the heap
pointer (pointer tag) are used to store a constructor identifier (0-7)
otherwise the constructor id is kept inside the closure requiring an
additional memory lookup.

Once the constructor is identified we need to jump to the target branch of a
case statement based on the constructor id. Depending on the number of
constructors and sparseness of the jump table it is either implemented as a
lookup table (array indexing) or as a binary search.

Physical Representation of ADTs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

TBD

..
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
~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~

The names or identifiers in one level (data, type or kind) should not be
confused or conflated with the names in other level. An identifier of the same
name can be used in different levels without any problem.

Tips: Understanding a Haskell Program
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Names of data constructor functions and types could be the same, which can be
confusing for beginners. Similarly type variables in type level and type
parameters in data level could be same or different, they should not be
confused with each other.

Basic Algebraic Data Types (Prelude)
------------------------------------

* TODO: provide links to the definitions in base
* Provide the definitions as well

+--------------------------------------------------------------------------------+
| import "base" Prelude                                                          |
+-------------+----------------------------------+-------------------------------+
| Type        | Values                           | Description                   |
+=============+==========+==========+============+===============================+
| ()          | ()       |          |            | Unit data type, empty tuple   |
+-------------+----------+----------+------------+-------------------------------+
| (a, b)      | (1, 'a') | (0.3, 1) | (1, 2)     | Two Tuple                     |
+-------------+----------+----------+------------+-------------------------------+
| Bool        | True     | False    |            | Boolean type                  |
+-------------+----------+----------+------------+-------------------------------+
| Ordering    |  LT      | EQ       | GT         | Comparison                    |
+-------------+----------+----------+------------+-------------------------------+
| Maybe a     | Nothing  | Just a   |            | Presence or absence           |
+-------------+----------+----------+------------+-------------------------------+
| Either a b  | Left a   | Right b  |            | Choice                        |
+-------------+----------+----------+------------+-------------------------------+

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
| Deadlock in pattern matches.                                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Count = Red Int | Green Int                                           |
|  count = Red 1                                                              |
+--------------------------------------+--------------------------------------+
| Let                                  | Where                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  let Red i | i < 5 = count           |  reds count = "R few" ++ show i      |
|  in "R few" ++ show i                |    where Red i | i < 5 = count       |
+--------------------------------------+--------------------------------------+
| This program results in a "case: <<loop>>" because i depends on itself!     |
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

Tuples
~~~~~~

* TBD
* TBD - tuple sections

References
----------

https://en.wikipedia.org/wiki/Pattern_matching
https://en.wikipedia.org/wiki/Proof_by_exhaustion case analysis
