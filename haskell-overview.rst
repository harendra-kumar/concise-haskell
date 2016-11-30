.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Haskell: A pure functional language
===================================

A functional language with denotational semantics.

Terminology
-----------

+------------------------+----------------------------------------------------+
| Functional language    | Functions are just like any other values, pass them|
|                        | as arguments or return them from functions, compose|
|                        | them together to create a composite function.      |
+------------------------+----------------------------------------------------+
| Value                  | In a given plane, refers to functions as well as   |
|                        | plane specific concrete objects. For example in a  |
|                        | data plane a value can refer to functions and data |
|                        | while in a type plane it can refer to functions    |
|                        | and types.                                         |
+------------------------+----------------------------------------------------+
| Denotational Semantics | constructing mathematical objects                  |
|                        | (called denotations) that describe the meanings of |
|                        | expressions from the programming language. An      |
|                        | important tenet of denotational semantics is that  |
|                        | semantics should be compositional: the denotation  |
|                        | of a program phrase should be built out of the     |
|                        | denotations of its subphrases.                     |
+------------------------+----------------------------------------------------+
| Equational Reasoning   |                                                    |
+------------------------+----------------------------------------------------+
| Referential            |                                                    |
| Transparency           | any variable can be replaced with its actual value |
|                        | at any point of execution (because the variable can|
|                        | never change)                                      |
+------------------------+----------------------------------------------------+
| Immutability           |                                                    |
+------------------------+----------------------------------------------------+
| Side effect            | A change in environment (global state, IO)         |
|                        | effected by a function                             |
+------------------------+----------------------------------------------------+
| Pure function          | A function which always returns the same value. A  |
|                        | function without a side effect.                    |
+------------------------+----------------------------------------------------+
| Pure functional        | No mutation of a data structure is allowed         |
+------------------------+----------------------------------------------------+
| Lazy evaluation        | Program execution is driven by IO, statements are  |
|                        | not executed unless they are needed by a           |
|                        | computation driven by IO. There is no sequential   |
|                        | evaluation of all statements in the control flow   |
|                        | path.                                              |
+------------------------+----------------------------------------------------+
| Bind                   | assign (bind) a value to a variable                |
+------------------------+----------------------------------------------------+
| Type signature         |                                                    |
+------------------------+----------------------------------------------------+
| Type annotations       |                                                    |
+------------------------+----------------------------------------------------+

Abstraction & Polymorphism
--------------------------

Polymorphism is a central concept in Haskell which is essentially a way to
reuse code. When you have  multiple `concrete` objects which are slightly
different from each other or have a common structure but differ only in some
parts then you can `abstract` all of them into one abstract form. You keep the
common parts as it is and replace the varying part with a variable parameter.
This abstract form is called a `polymorphic` form or an `abstraction` since
it is a representation of many different concrete forms. This phenomenon is
called `abstraction` or `polymorphism`.

In a polymorphic or abstract form we can replace the variable parameter with a
desired value to arrive at a concrete form. By using different values of the
parameter we can create all the different concrete forms. The process of
supplying the values of parameters to create a concrete form is called
`instantiation` of the polymorphic form or creating an `instance`.
Instantiation process is the opposite of abstraction.

Even an abstract form can be further abstracted creating higher level
abstractions.

Functions As Building Blocks
----------------------------

Values & Functions
~~~~~~~~~~~~~~~~~~

Summarizing picture::

  Value    -------> abstraction ------> abstract value
  Concrete <------- application <------ function
           <------- currying

+-----------------------------------------------------------------------------+
| A `concrete value` is an expression which can be computed to a pure data    |
| without requiring any input (or variables). It is specified as an equation. |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  v0 = 10                                                                    |
|  v1 = 10 + 20                                                               |
+-----------------------------------------------------------------------------+

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
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f0       = 10 + 10 + 10  -- normal value                                   |
|  f1 a     = a  + 10 + 10  -- polymorphic value of arity 1                   |
|  f2 a b   = a  + b  + 10  -- polymorphic value of arity 2                   |
|  f3 a b c = a  + b  + c   -- polymorphic value of arity 3                   |
+-----------------------------------------------------------------------------+
| Abstraction allows `reuse` of the same value for multiple purposes.         |
+-----------------------------------------------------------------------------+
| `a`, `b` and `c` are variable parameters. The abstract value can have       |
| different concrete `instances` for different values of the parameters.      |
+-----------------------------------------------------------------------------+


+-----------------------------------------------------------------------------+
| A `function` is a polymorphic value, an abstraction.                        |
| A function is defined by an equation like this:                             |
+------+------------+---+-----------------------------------------------------+
| Name | Parameters | = | Body                                                |
+------+------------+---+-----------------------------------------------------+
| f    | a b c      | = | <expression> -- function defining equation          |
+------+------------+---+-----------------------------------------------------+
| Parameters `a`, `b` and `c` are bound to the input arguments when the       |
| function is applied.                                                        |
+-----------------------------------------------------------------------------+
| `Arity` is the number of parameters of a function.                          |
+-----------------------------------------------------------------------------+
| A function can be thought of as an `incomplete value` which becomes         |
| complete or concrete when the real values of abstracted variables are       |
| applied to it.                                                              |
+-----------------------------------------------------------------------------+
| In mathematical terms a function maps the input `values` (or arguments)     |
| to an output `value`.                                                       |
+-----------------------------------------------------------------------------+
| The name of a function necessarily starts with a lower case letter.         |
+-----------------------------------------------------------------------------+

::

     +-  -  -  -+
     | \/ \/ \/ |
  f  | a  b  c  |
     |          |
     | Value    |
     +----------+
       Arity 3

+-----------------------------------------------------------------------------+
| A function application is opposite of abstraction i.e. it concretizes the   |
| value by removing abstraction.                                              |
+-----------------------------------------------------------------------------+
| A `function application` combines a function value with its input(s) or     |
| arguments to generate an output value.                                      |
+--------+---+------+---------------------------------------------------------+
| Output | = | Name | arguments                                               |
+--------+---+------+---------------------------------------------------------+
| v4     | = | f    | v1 v2 v3  -- function application equation              |
+--------+---+------+---------------------------------------------------------+
| Note: Function definition and application are complementary concepts. The   |
| LHS of a definition becomes application on RHS, the parameters are replaced |
| by input argument values.                                                   |
+-----------------------------------------------------------------------------+
| Function arguments can be applied one at a time, called `currying`.         |
+-----------------------------------------------------------------------------+
| Each argument application reduces the arity of the function by one or in    |
| other words it produces another function as output whose arity is one less  |
| than the original arity. In other words the function value gets refined     |
| to a less abstract value.                                                   |
+-----------------------------------------------------------------------------+
| When a function is fully applied with all its arguments it turns into a     |
| fully concrete value.                                                       |
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

Values
~~~~~~

+-----------------------------------------------------------------------------+
| A function application concretizes or refines the abstract value            |
| represented by the function by composing the function with the values of    |
| of its parameters.                                                          |
+-----------------------------------------------------------------------------+
| When we say `value` in general we mean anything that a function can accept  |
| as its arguments.                                                           |
+-----------------------------------------------------------------------------+
| A value could be a `concrete value` or a `function`.                        |
+-----------------------------------------------------------------------------+

Higher-order functions
~~~~~~~~~~~~~~~~~~~~~~

+---------------+------------------------------------------------------+
| first order   | arguments and return values are concrete             |
+---------------+------------------------------------------------------+
| second order  | Arguments or return value is a function              |
+---------------+------------------------------------------------------+
| third order   | Arguments or return value is a second order function |
+---------------+------------------------------------------------------+

Data Level Program
------------------

The basic purpose of a Haskell program is to take input data and produce
output data. The fundamental tool used to acheive that is `functions` and
`function application`. Here we will discuss how to implement a function by
representing pure data (input and output) and mapping input data to output
data.

In a data level program (the direct and main aspect of a Haskell program)
functions operate on values which could be either functions or data.

Constructing Data By Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only way to construct a new or user defined data representation is to call
a function using existing data representations as arguments.

Explain sum, product and recursive algebraic data types.

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
| The name of a data constructor necessarily starts with an upper case letter.|
+-----------------------------------------------------------------------------+

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

::

  Possible "Color" values:

      +----------+            +----------+            +----------+
      |          |            |          |            |          |
      |          |            |          |            |          |
  Red |          |      Green |          |       Blue |          |
      |          |            |          |            |          |
      +----------+            +----------+            +----------+

::

  Possible "Triple" values:

         +----------+              +----------+
         |          |              |          |
         |          |              |          |
  Triple | 1 2 3    |       Triple | 10 20 30 |
         |          |              |          |
         +----------+              +----------+

Examining Data By Pattern Matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
|    Red   -> putStrLn "red"                                                  |
|    Green -> putStrLn "green"                                                |
|    Blue  -> putStrLn "blue"                                                 |
+-----------------------------------------------------------------------------+
| When the value `color` is `Red` this expression will evaluate to            |
| `putStrLn "red"`                                                            |
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
expressed in terms of these primitives`.

Transforming Data By Composing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the output of a function as input of another function to transform
data.

Function and Data Values
~~~~~~~~~~~~~~~~~~~~~~~~

* A function is an abstract value, a map from values to values while data is a
  container of values, concrete or abstract. It does not make sense to pattern
  match on an abstract value.
* Data is a concrete structure to hold any type of values (abstract or
  concrete) and give them back by pattern match.
* A concrete value at data level is always pure data.

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

Data Constructor Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Data constructor signatures are not specified directly but through a data
declaration. A data declaration specifies a data type on the LHS and
constructor templates on the RHS.

+---------------------------------------------------------+-----------------------------------------------+
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

Plugs and Sockets
~~~~~~~~~~~~~~~~~

If value is a plug and the function input is a socket, the type checker makes
sure that the plug correctly fits into the socket. The Haskell program is a
zigsaw puzzle of different types of plugs and sockets.

`Inference`: If two plugs fit into the same socket then they must be of the
same type. If two sockets accept the same plug then they must be of the same
type.

Insert graphic plug and socket.
Insert "input >=> output" Haskell program zigsaw puzzle.

Type Checking
~~~~~~~~~~~~~

The onus of assigning unique types to different data items is on the programmer
so that they do not get confused by mistake.  The type annotations for values
in data level program can collectively be thought of as a `type level program`.

The type level program is interpreted at compile time by the `typechecker`.  It
essentially checks if the types used in the data level program are consistent
with the type level program. Some fundamental checks:

* `functions`: The only way to compose values is a function. The type of the
  function input must match the type of the value being fed to the function.

* `case`: The only way a function maps one type to another is via case
  expression. All the values mapped from must have one type and all the values
  mapped to must have one type.

* `Equations`: When two values can be substitued in place of each other then
  they must have the same type.

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
value. The combinations are huge, so how do we create so many types?

+-----------------------------------------------------------------------------+
| We generate the function types using a type level function denoted by       |
| ``->``. This is a GHC built-in.                                             |
+------------------+----------------------------------------------------------+
| (->) a b         | Returns a concrete type representing a data function     |
+------------------+ which takes a data value of type `a` as argument and     |
| a -> b           | returns a data value of type `b`.                        |
+------------------+----------------------------------------------------------+
| (->) a ((->) b c)| Returns a concrete type representing a data function     |
+------------------+ which takes a data value of type `a` as argument and     |
| a -> b -> c      | returns a function of type ``(b -> c)``.                 |
+------------------+                                                          |
| a -> (b -> c)    |                                                          |
+------------------+----------------------------------------------------------+

Kinds: Ensuring correctness of Types
------------------------------------

+-----------------------------------------------------------------------------+
| Safety of type level programming is ensured by labeling types with different|
| `kinds` and performing a `kind check` when a type function is applied.      |
| Kinds are relatively few and classified as follows:                         |
+------------------------+----------------------------------------------------+
| Concrete or abstract                                                        |
+------------------------+----------------------------------------------------+
| Concrete types         | Type functions                                     |
+------------------------+----------------------------------------------------+
| Runtime representation | Arity                                              |
+----------+-------------+------------------+---------------------------------+
| Unlifted | Lifted      | 1                | ...                             |
+----------+-------------+------------------+---------------------------------+
| ...      | ``Type``    | ``Type -> Type`` | ...                             |
+----------+-------------+------------------+---------------------------------+

+-----------------------------------------------------------------------------+
| A `kind signature` assigns a kind to each parameter of a type function.     |
+-----------------------------------------------------------------------------+
| `Kind check` fails if we pass the wrong kind to a type function.            |
+-----------------------------------------------------------------------------+
| For example the kind signature of type function ``->`` is::                 |
|                                                                             |
|  (->) :: Type -> Type -> Type                                               |
+-----------------------------------------------------------------------------+
| We cannot pass an unlifted type (e.g. Int#) or a type function (e.g. a type |
| of kind ``Type -> Type``) to this function.                                 |
+-----------------------------------------------------------------------------+

.. _RuntimeRep: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#runtime-representation-polymorphism

+------------------------------------------------------------------------------------------------------------------------------+
| A concrete type's kind encodes the runtime representation.                                                                   |
+----------------------+----------------------+--------------------------------------------------------------------------------+
| Kinds                | Unlifted Types       | ``TYPE 'IntRep'``, ``TYPE 'DoubleRep'`` ...                                    |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Lifted Types         | ``Type`` or ``*``                                                              |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Constraints          | ``Constraint``                                                                 |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Type level naturals  | ``Nat``                                                                        |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Type level symbols   | ``Symbol``                                                                     |
+----------------------+----------------------+--------------------------------------------------------------------------------+
| GHC internally represents a kind as ``TYPE`` parameterised by `RuntimeRep`_.                                                 |
+------------------------------------------------------------------------------------------------------------------------------+
| ``Type`` (Post GHC 8.0 only) or ``*`` is the only kind visible outside GHC, and defined as:                                  |
| ``type Type = TYPE 'PtrRepLifted'``                                                                                          |
+------------------------------------------------------------------------------------------------------------------------------+

Polymorphic Functions
---------------------

Functions whose argument types can vary. They work for many types.

::

  id :: a -> a
  id x = x

`Function instances`: When we apply the identity function to a value of a
concrete type, then we `instantiate` the type variable `a` to that concrete
type:

::

  id (3 :: Int)

This is also known as `parametric polymorphism`.

Quantification of Type Variables
--------------------------------

Quantification decides the `visibility scope of a type variable` to the
typechecker. The type variable cannot be instantiated and cannot exist
outside that scope. There are two types of quantifications available viz.
`univseral` and `existential` quantification.

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

Type Level Polymorphism
~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+--------------------------------------------------------------------------------+------------------+
| Polymorphic Type Fns | ``t :: k1 -> k2``, where k1 is a kind variable representing types of rank0     |                  |
+----------------------+--------------------------------------------------------------------------------+------------------+
| Type Functions       | ``t :: Type -> Type``                                                          | Polymorphic type |
+----------------------+--------------------------------------------------------------------------------+------------------+
| Concrete Types       | ``t :: Type``                                                                  | Monomorhic type  |
+----------------------+--------------------------------------------------------------------------------+------------------+

Data Level Polymorphism
~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+--------+--------------------------------------------------------------------------------+-------------------------+
| Polymorphic Functions| Rank3  | ``f :: (Rank2 polymorphic function type) -> b``                                | Abstract functions      |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | f :: ((forall a. a -> a) -> Int) -> Int                                        |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | Rank can be determined by counting the nesting depth of the type variable      |                         |
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

Summary of Programming Levels
-----------------------------

+--------------+---------------------------+-------------+----------------------------------------------------+
| When         | What                      | Objects     | Haskell Program Features                           |
+==============+===========================+=============+====================================================+
| Compile time | `Kind` level programming  | Kinds       | Kind Signatures                                    |
|              +---------------------------+-------------+----------------------------------------------------+
|              | `Type` level programming  | Types       | Function Type Signatures                           |
|              |                           |             +----------------------------------------------------+
|              |                           |             | Data Constructor Signatures                        |
|              |                           |             +----------------------------------------------------+
|              |                           |             | Typeclasses (Function & Data signatures)           |
+--------------+---------------------------+-------------+----------------------------------------------------+
| Run time     | `Data` level programming  | Data        | Concrete data values, Functions, Data Constructors |
+--------------+---------------------------+-------------+----------------------------------------------------+

General Model of a Haskell Program
----------------------------------

A Haskell program is essentially a function called `main` which `maps` input
`values` of the program to output `values` potentially via intermediate
functions.

If you flatten a Haskell program it can be thought of just as a big map, each
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
* There are three independent functional programming spaces viz. value, type
  and kind
* The bridge between any two spaces is a function name
