.. raw:: html

  <style> .blue {color:blue} </style>

.. role:: blue

Functions: Abstract Expressions
===============================

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
| Parameters             | The variables denoting arguments of a function     |
+------------------------+----------------------------------------------------+
| Arguments              | Actual parameter values supplied in a function call|
+------------------------+----------------------------------------------------+
| Arity                  | The number of parameters of a function             |
+------------------------+----------------------------------------------------+
| Nullary                | A function or constructor with arity 0             |
+------------------------+----------------------------------------------------+
| Unary                  | A function or constructor with arity 1             |
+------------------------+----------------------------------------------------+
| Binary                 | A function or constructor with arity 2             |
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
| Concrete               | Represents a value without any unknowns            |
|                        | (not abstract)                                     |
+------------------------+----------------------------------------------------+
| Monomorphic            | Has only one possible concrete representation      |
+------------------------+----------------------------------------------------+
| Polymorphic            | Has multiple concrete representations (abstract,   |
|                        | not concrete)                                      |
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
many different concrete forms. However the term polymorphic is commonly used
only when we are talking about abstraction of types instead of terms.

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

Instantiation
~~~~~~~~~~~~~

The process of supplying the values of parameters to create a concrete form is
sometimes called `instantiation` of the polymorphic form or creating an
`instance`.  Instantiation process is the opposite of abstraction.

::

  red ball, green ball, blue ball => ball + color => instances

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

Abstracting: Defining a Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will now see that functions are a very powerful and convenient tool for
abstraction.  Writing a haskell program is in fact a process of abstraction
that the programmer goes through.  The functions defined in a program are a
result of that abstraction.  This abstraction process is also called `beta
abstraction` in `lambda calculus` terminology.

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

Function Application: Currying
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
| have distinct roles, which means the operation is not commutative i.e.      |
| `f a` is not the same as `a f`                                              |
+-----------------------------------------------------------------------------+
| This operation is left associative i.e. ``f a b c <=> ((f a) b) c``         |
+-----------------------------------------------------------------------------+

Function Application: Reverse
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two ways to think about a function application.  The first is
applying a function to a value (``f $ x``) and the other applying a value to a
function (``($ x) f``).  Function eats value or value eats function, its the
same thing. However to make the value eat function we have to turn it into a
function.  Former is the more common way and gives rise to curried functions
whereas the latter is less common and gives rise to continuations.  Both are
duals to each other, both give rise to higher order functions in a dual or
opposite sense.  The first one returns function as values and the second one
takes functions as arguments.

What is a Function?
~~~~~~~~~~~~~~~~~~~

From an abstraction standpoint a function is an abstraction of an expression.
From a mathematical standpoint a function is a mapping of values of one type or
a combination of types (input types) to values of another (output) type. The
implementation of a function in Haskell is very close to its mathematical
definition, as we will see we `case analyze` the input types and map the
individual values or groups of values to output types. But before we can
understand that we need to understand `Algebraic Data Types`.

In an expression functions can originate only from two sources, a static
function definition or as an output of another function. A function can return
a function when it is partially applied or by defining a new function using a
`lambda expression` which defines an anonymous function.

.. By its mathematical definition, the domain of a function is more general,
  i.e. has equal or more choices than the codomain that is being mapped to.
  Therefore the domain is an abstraction or generalisation of the codomain.
  When we map using a function the target choices will always be less than or
  equal to the choices in the source.  Therefore, transformation and
  abstraction are opposite processes and may not be reversible.

Defining Functions
------------------

Function classes
~~~~~~~~~~~~~~~~

Parameter structure independent functions (pure composition)
Parameter structure aware functions (ad-hoc)

Parameter type independent functions (parametric polymorphism)
Parameter type aware functions (ad-hoc polymorphism)

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
de-structure it.

Defining Functions
~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| We have already seen function application, definition is just the opposite. |
+------------------------------------+----------------------------------------+
| Application                        | Definition                             |
+====================================+========================================+
| v = f x y z                        | f a b c = ...                          |
+------------------------------------+----------------------------------------+

+-----------------------------------------------------------------------------+
| Function definition equations                                               |
+-----------------------------------------------------------------------------+
| ``square n = n * n``                                                        |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = square x + square y``                                  |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = let square n = n * n in (square x + square y)``        |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = (square x + square y) where square n = n * n``         |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| When the RHS of the equation is a function application then we can omit     |
| trailing parameters that are identical on both sides.                       |
+-------------------------------------+---------------------------------------+
| Explicit definition                 | Equivalent definition                 |
+-------------------------------------+---------------------------------------+
| ``f a b = g a b``                   | ``f = g``                             |
+-------------------------------------+---------------------------------------+
| ``f a b = g (a + 1) b``             | ``f a = g (a + 1)``                   |
+-------------------------------------+---------------------------------------+
| When ambiguous always imagine that there are parenthesis around RHS         |
+-------------------------------------+---------------------------------------+
| ``f a b = g (5 + 5) b``             | ``f = g $ 5 + 5``                     |
+-------------------------------------+---------------------------------------+
| ``f a = print $ (+) 5 a``           | ``f = print $ (+) 5`` -- INCORRECT    |
|                                     +---------------------------------------+
|                                     | ``f = print . (+) 5`` -- CORRECT      |
+-------------------------------------+---------------------------------------+

+-----------------------------------------------------------------------------+
| Variables capture and shadowing (terminology)                               |
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

Anonymous Functions
^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| A lambda or an anonymous function is an expression denoting a function. It  |
| allows you to define a function in-place inside an expression.              |
+-----------------------------------------------------------------------------+
| ``\a b c -> ...``                                                           |
+-----------------------------------------------------------------------------+
| ``let sumOfSquares f x y = f x + f y in sumOfSquares (\n -> n * n) 3 4``    |
+-----------------------------------------------------------------------------+
| Without explicit parentheses, a lambda extends all the way to the end of    |
| the expression.                                                             |
+-----------------------------------------------------------------------------+

Case-mapped Functions
~~~~~~~~~~~~~~~~~~~~~

Previously we defined simple functions that were merely a composition, or
expressions involving other existing functions. A real primitive function is
created by a `case analysis` on the input and thereby mapping different values
of the input data type to different values in the output data type. This
requires three fundamental tools, `pattern matching` to destruct the input
data, `case statement` to map inputs to outputs and `data constructors` to
create new output data type.

+--------------------------+---------------------+----------------------------+
| Data Level               | Bridge              | Type Level                 |
+==========================+=====================+============================+
| Data construction        |                     |                            |
+--------------------------+                     |                            |
| Case analysis            | Data declaration    | Algebraic Data Types       |
+--------------------------+---------------------+----------------------------+

Multi Equation Function Definitions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A case-mapped function can be defined more naturally as multiple equations. Each
equation defines the function for a certain input pattern by using a pattern
match on its arguments.  This is just a syntactic sugar on a `case` pattern
match.

+--------------------------------------+--------------------------------------+
| Function                             | Case                                 |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  name Red   i = "R " ++ show i       |  name c = case c of                  |
|  name Green i = "G " ++ show i       |    Red   i -> "R " ++ show i         |
|                                      |    Green i -> "G " ++ show i         |
+--------------------------------------+--------------------------------------+
| All equations of a function must remain together i.e. no other definition   |
| can come between them.                                                      |
+-----------------------------------------------------------------------------+
| Just like `case` alternatives, patterns in equations are matched from top   |
| to bottom.                                                                  |
+-----------------------------------------------------------------------------+
| Multi equation functions can also be defined inside `let` and `where`       |
| clauses.                                                                    |
+-----------------------------------------------------------------------------+

Type Level Syntax
-----------------

The purpose of type level programming is to generate concrete types to be used
in the data level program.  Just like at data level we create `data functions`
representing `asbtract` or `polymorphic data`, the same way at the type level
we can create `type functions` representing abstract or `polymorphic types`.
Type functions can be used to compose types together to generate more complex
types from simple concrete types.

Note that the type assigned to any data level value is always `concrete`.  The
type of a data value can never be a type function. Type functions only exist at
the type level. See the kinds section for details.

Function Type Signatures
~~~~~~~~~~~~~~~~~~~~~~~~

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

Type Operator ``->``
~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Type level expressions representing complex types can be created by         |
| combining simple types using type level operators.                          |
+-----------------------------------------------------------------------------+
| ``->`` is a right associative type operator which is used to generate type  |
| signatures of functions. ``->`` generates a function's type from the        |
| function's `argument type` and `return type`.                               |
+-----------------------------------------------------------------------------+
| A function taking an `Int` argument `x` and returning an `Int`:             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  inc :: (->) Int Int    -- function form                                    |
|  inc :: Int -> Int      -- operator form                                    |
|  inc x = x + 1                                                              |
+-----------------------------------------------------------------------------+
| A multi argument function is really a single argument function returning    |
| another function which consumes the rest of the arguments.                  |
| A function taking two `Int` arguments `x` and `y` and returning an `Int`:   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  add :: (->) Int ((->) Int Int)  -- function form                           |
|  add :: Int -> (Int -> Int)      -- explicit right associative form         |
|  add :: Int -> Int -> Int        -- commonly used infix form                |
|  add x y = x + y                                                            |
+-----------------------------------------------------------------------------+

Products, Exponentials and Logarithms
-------------------------------------

Pure Combine: Free Products
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A pure product data type of two types can be called a "free product". For
example a free product of two integers can be written as::

  data Pair = Pair Int Int

``Pair`` is free from any "interpretation" and we can write our own interpreter
or specific implementation of the product::

  add (Pair a b) = a + b
  multiply (Pair a b) = a * b

`add` and `multiply` are two different ways to interpret the free product type
``Pair``, we can have many more. `Pair` is pure data and the interpretation
adds logic to interpret that data.

In contrast to a free product are interpreted products or just curried
functions. Every multiple argument function is a product of its arguments. For
example the function ``(+) a b`` generates a type that is a product of input
types but it is not a free product because it has an interpretation of adding
two numbers, built into it.

Pure Transform: Exponentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A unary function.

Transform and Combine Adjunction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are using the term adjunciton loosely here but we will use it more formally
when we talk about functors.

N-ary function:
  put n things together : map the composite to another

..
  We can first compose and then map. Is it possible to first map and then
  compose?
    a x b -> c               -- just like NOT (a AND b) = (NOT a) OR (NOT b)
    (a -> c1) + (b -> c2)    -- that's why NOT is always needed and one of and/or
                             -- sum of partial applications
    i.e. c = c1 + c2         -- Function is like the log operation?

    We can illustrate the above with an example - (Color, Size) and convert
    that to strings e.g. "big red".

    i.e. mapping of a product is a sum of mappings
    this is just for intuition. It may not be possible for a map like a
    -> c to even exist even though (a, b) -> c exists.

    Is this possible to prove it this way?
      when we give a to a x b -> c it produces b -> c
      when we give b to a x b -> c it produces a -> c
      Now that we have given both the inputs separately and got the results we
      can combine the two complementary results to arrive at the final result.

    Yoneda Lemma says values are functions and functions are values. Using
    representable we can translate between the two. (->) is representable
    (distributive) as we know by definition it is mapping from values to values
    which is the representation.

  Transformation and Combining, Nesting?
  --------------------------------------

  f = (g x (h y (k z)))
  f x = (h y (k z))
  f x y = (k z)
  f x y z = ...

  A multi-arity function is nested transformations. Each value has an associated
  transformation. We apply the value and its transformation and then over all
  the resulting values we apply another transformation modulated by another value
  and so on.

  Therefore a product type embodies the idea of nesting. A multi-arity function
  corresponds to a product, it applies nested transformations. We see the dual of
  this nesting when we use funciton arguments to a function.

  Currying basically removes one nest layer and then returns the rest of the
  nesting i.e. a function with one less argument.

  The two ways
  ------------

  A function can be thought of in two different ways, in one you transform and
  collapse in each step and the other you transform all and then combine all.
  Notice the similarity between the collapsing of a nest layer with a Monad.

  transform, transform, transform => combine (sum) all components monoidally
  transform, combine, transform, combine, transform - this is more monadic style
  in nature as you can transform and combine in lockstep.
  So there are two distinct logic components here, one for transform and the
  other for combine.

  Product style nesting or sum style folding
  -------------------------------------------

  There are two ways of thinking about composing and both of them commute into
  each other. Any abstraction can be thought of in any of these two ways. Either
  nested transform and combine operations or a series of transforms followed by a
  series of sums. So the basic operations are transform and (sum or product).

  N-ary functions
  ~~~~~~~~~~~~~~~

  Free application vs. curried application.
  An n-ary function as a nested case construct.

Consumers and Producers
-----------------------

In pure programming there are either functions that consume one or more inputs
and produce one output or there are values (non-functions) that neither consume
nor produce.

.. TBD depict with pictures.

+---------+---------+------------------------+
| Consume | Produce | Object                 |
+=========+=========+========================+
| N       | N       | Non-function value     |
+---------+---------+------------------------+
| N       | Y       | Does not exist         |
+---------+---------+------------------------+
| Y       | N       | Does not exist         |
+---------+---------+------------------------+
| Y       | Y       | function               |
+---------+---------+------------------------+

Unary Transforms
----------------

A `transform` operation is an abstract notion, and the simplest possible
operation on a single value.  It transforms or maps a value in one domain to a
corresponding value in another domain.

An example of such the operation is `colorCode` that maps a color from the
`Color` domain to a number in `Integer` domain::

  colorCode :: Color -> Integer

  colorCode Red   = 1
  colorCode Green = 2
  colorCode Blue  = 3

Another example is the operation `succ` that returns the successor of a number.
It maps a number from the `Integer` domain to its successor in the same domain::

  succ :: Integer -> Integer

  succ 1 = 2
  succ 2 = 3
  ...

Think about ``colorCode`` and ``succ`` as abstract notions representing a
mapping or transformation from one type of value to another type. We call them
`unary functions` or `unary operations` in mathematical parlance.

General Unary Functions
~~~~~~~~~~~~~~~~~~~~~~~

In Haskell, the domains are Haskell types. A general unary function would
transform a type `a` into another type `b`.  We can represent such a function
by the following type signature::

  f :: a -> b
  f :: a -> a

What can we do with them?
~~~~~~~~~~~~~~~~~~~~~~~~~

It is interesting to see how we can use a function without knowing the specific
type of values that it works on.

In what ways can a unary operation be useful? What can we do with a single
value? We can apply a unary operation to transform it.

Single function, single value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`f :: a -> b` can just transform a value in another type and that's it.
`f :: a -> a` is more interesting. Since the output is of the same type we can
use the same function on output again. This is called iteration. If we keep on
feeding the output to the same function we will either converge or not. We
can only converge if f x = x for some x. This is called the fixed point of the
function.

Set of functions, single value
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can apply another unary operation on the resulting value to transform it
again and so on. For example::

  colorCode Green => 2
  succ 2          => 3

Single function, Set of values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If we have a collection of values of a given type we can think of transforming
the whole collection into a new collection of a different type. For example::

  colorCode <$> [Red, Green, Blue] => [1, 2, 3]
  succ      <$> [1, 2, 3]          => [2, 3, 4]

Binary functions
----------------

In Haskell we usually build bigger structures using binary operations. For
example the canonical product and sum types are (a,b) and Either a b
respectively. We can build bigger product and sum types by just combining them
using these two types! For example, functions are composed in a binary fashion
using currying. async package combines fork in a binary fashion to implement
parallel applicative or race alternative.

Combine Two
~~~~~~~~~~~

A combine operation is an abstract notion, that takes two values from two
domains and combines them in some way to produce a value in a third domain. In
other words, it transforms a combination of two values to a third value in
another domain.

A simple example of a binary operation is an addition operation which adds two
numbers, both in the number domain, and produces a third number which is called
the sum of the two numbers, also in the number domain::

  add :: Integer -> Integer -> Integer

  add 1 1 = 2
  add 1 2 = 3
  ...
  add 2 1 = 3
  add 2 2 = 4
  ...

Another example, is a tuple operation::

  tuple :: String -> Integer -> (String, Integer)

  tuple "a" 1 = ("a",1)
  tuple "b" 2 = ("b",2)
  ...

Think about ``add`` and ``tuple`` as abstract notions combining two types of
values into a value of third type. We call them `binary functions` or `binary
operations` in mathematical parlance.

In Haskell, a general binary function would transform a type `a` and a type `b`
into third type `c`.  We can represent such a function by the following type
signature::

  f :: a -> b -> c

Any two types or all the three types could be different or the same::

  f :: a -> a -> a
  f :: a -> a -> b
  f :: a -> b -> a
  f :: a -> b -> b

How is a binary operation useful?

Church Encoding
---------------

The church and unchurch functions convert between nonnegative integers and
their corresponding Church numerals.

::

  type Church a = (a -> a) -> a -> a

  church :: Integer -> Church Integer
  church 0 = \f -> \x -> x
  church n = \f -> \x -> f (church (n-1) f x)

  unchurch :: Church Integer -> Integer
  unchurch cn = cn (+ 1) 0

The term "church encoding" is used in a more general sense. For example, see
this doc in the streaming package:

::

  destroy :: (Functor f, Monad m) => Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b

  Map a stream directly to its church encoding.

Operational Aspects
-------------------

Currying
~~~~~~~~

Operational aspects, how currying is implemented.
Implications of curried functions vs fully applied functions on performance.
Currying is like a binary operation, and uncurried like a free operation.
Currying is like repeated transformation.

The way full application can provide better performance in certain situations a
free structure should be able to provide better performance compared to
a non-free structure. What are those for applicative/monad? The function
application impl is different because we can pass the arguments in registers
when fully applied and passing arguments one at a time with currying could be
expensive.

References
----------

* https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser
* http://conal.net/blog/posts/everything-is-a-function-in-haskell
* http://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf Datatype-Generic Programming
