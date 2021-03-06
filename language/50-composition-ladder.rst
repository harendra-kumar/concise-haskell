Composition Ladder
==================

Computation Primitives
----------------------

`Transformation` is mapping from one type to another and `composition` is
putting multiple types together as a `product` type.  We are going to describe
all computation in these two conceptual primitives.

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

Combining
~~~~~~~~~

Composition is a general way of combining multiple objects of potentially
different types into a single object. Later we will discuss many interesting
special cases where we can abstract composition of arbitrary number of objects
using recursion and a binary or n-ary composition operation.

N-ary Product Composition
~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Composes a finite number (not a stream) of  objects of potentially          |
| different types.                                                            |
+================+============================================================+
| N-ary          | A constructor just stores multiple data types together as  |
| constructor    | a product type.                                            |
|                +------------------------------------------------------------+
|                | ``C :: A -> B -> C``                                       |
+----------------+------------------------------------------------------------+
| N-ary function | Transforms an implicit input product type to an output     |
|                | type. We can think of an N-ary function in uncurried form  |
|                | i.e. a unary function accepting a pre-composed product     |
|                | type.                                                      |
|                +------------------------------------------------------------+
|                | ``f :: A -> B -> C``                                       |
|                +------------------------------------------------------------+
|                | ``f :: (A, B) -> C``                                       |
+----------------+------------------------------------------------------------+
| An n-ary function has the big picture of all its arguments; and ultimately  |
| translates to a tree of case expressions, on its way down the tree it       |
| destructures the input types and constructs the output type.                |
| A composed n-ary function just routes its arguments to right places in      |
| an expression. TBD provide examples.                                        |
+-----------------------------------------------------------------------------+

Functions and Constructors
--------------------------

We treat an n-ary function conceptually as a combination of these two
operations, ignoring currying as an implementation convenience.

+-----------------------------------------------------------------------------+
| Functions and constructors are duals of each other.                         |
+===================================+=========================================+
| Function (Destructor)             | Constructor                             |
+-----------------------------------+-----------------------------------------+
| Unary: Identity compose,          | Product: Compose, identity transform    |
| transform (pure transformation)   | (pure composition)                      |
+-----------------------------------+-----------------------------------------+
| N-ary: Product compose, transform | Sum: Tag, identity transform            |
+-----------------------------------+-----------------------------------------+
| Consume-produce-consume...        | Produce-consume-produce...              |
| (Starts with consumption)         | (Starts with production)                |
+-----------------------------------+-----------------------------------------+
| Mealy Machine                     | Moore Machine                           |
+-----------------------------------+-----------------------------------------+

Abstraction
-----------

Abstraction is applied to both data and type spaces.  The most basic
abstraction is function. In fact, any abstraction can in general be equated
with a function or mapping.

Functions are further abstracted using parametric and ad-hoc polymorphism.
Typeclasses and type families provide facilities to implement ad-hoc
polymorphism in Haskell.

The fundamentals of abstraction are studied formally in a branch of mathematics
called `lambda calculus`.  Abstraction facilities in Haskell are discussed in
detail in XYZ.

Abstraction vs Performance
--------------------------

An abstraction is also an indirection in terms of implementation. An
indirection creates a performance overhead when the indirection is a runtime
indirection and not just compile time. For example a function creates an
indirection of a function call.  A data constructor creates a layer of
indirection. A free monad creates another layer of indirection compared to a
monad. However the type level abstraction is static and mostly can be removed
at the compile time. But if we use free structures we add a runtime abstraction
too.

However, we should note that performance impacts are not always intuitive or
easily quantified. In a given scenario performance may not be important or the
impact may not be significant because of the 80/20 rule.

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
|                                     | ::                                    |
|                                     |                                       |
| Concrete Terms                      |  'a', 5, "hello"                      |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Concrete expressions                |  5 + 4                                |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Composed functions                  |  f x y = x + y                        |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
|                                     |  f :: Int -> String                   |
| Pattern matched functions           |  f x = case x of                      |
| (case analysing)                    |    1 -> "one"                         |
|                                     |    _ -> "any"                         |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
|                                     |  f :: a -> a                          |
| Parametrically polymorphic functions|  f x = x                              |
+-------------------------------------+---------------------------------------+
| Ad-hoc polymorphism                 |                                       |
+-------------------------------------+---------------------------------------+

Type Level Abstraction Ladder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------------------------+---------------------------------------+
| Description                         | Example                               |
+=====================================+=======================================+
|                                     | ::                                    |
|                                     |                                       |
| Concrete primitive types            |  Int, Char, ...                       |
+-------------------------------------+---------------------------------------+
| Concrete/Monomorphic types          | ::                                    |
| (Algebraic Data Types/              |                                       |
| user defined)                       |  data Color = Red | Green | Blue      |
+-------------------------------------+---------------------------------------+
| Expressions                         | ::                                    |
|                                     |                                       |
|                                     |  Int -> Int, [Int] ...                |
+-------------------------------------+---------------------------------------+
| Type functions (parametric)         | ::                                    |
|                                     |                                       |
|                                     |  data Pair a = Pair a a               |
+-------------------------------------+---------------------------------------+
|                                     | ::                                    |
|                                     |                                       |
| Type Functions (pattern matched)    |  data family Pair                     |
| (type families)                     |  Pair () = Pair                       |
|                                     |  Pair Int = Pair Int Int              |
+-------------------------------------+---------------------------------------+
| Kind Polymorphic Type Functions     |                                       |
| (e.g. ``t :: k1 -> k2``)            |                                       |
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

Polymorphism
------------

Terminology
-----------

+-------------------------+---------------------------------------------------+
| Polymorphism            | A value or function can have multiple types       |
+-------------------------+---------------------------------------------------+
| Parametric Polymorphism | The type of a value is defined in terms of a type |
|                         | parameter or variable.                            |
+-------------------------+---------------------------------------------------+
| Ad-hoc Polymorphism     | A value is given different definitions depending  |
|                         | on the type                                       |
+-------------------------+---------------------------------------------------+
| Bounded Polymorphism    | Another name for ad-hoc polymorphism, bounded     |
|                         | because it is limited by the instances or         |
|                         | definitions.                                      |
+-------------------------+---------------------------------------------------+
| Parametric type         | A data, type synonym, data family or type family  |
|                         | parameterized by a type variable.                 |
+-------------------------+---------------------------------------------------+
| Polymorphic type        | Parametrically polymorphic type with a single     |
|                         | representation of constructors based on the       |
|                         | paremeter.                                        |
+-------------------------+---------------------------------------------------+
| Data families           | Parametric data types that can be assigned        |
|                         | specialized constructor representations based on  |
|                         | the type parameters they are instantiated with    |
+-------------------------+---------------------------------------------------+
| Type synonym families   | Parametric type synonyms that can be assigned     |
|                         | specialized representations based on the type     |
|                         | parameters they are instantiated with             |
+-------------------------+---------------------------------------------------+
| Type families           | Data families and type synonym families together  |
|                         | can be referred to as type families.              |
+-------------------------+---------------------------------------------------+

Polymorphism - Expressive Power
-------------------------------

* Typeclasses: attach functions to types, the function can be different for different types.
* Polymorphic functions: Same function can work on many types
* Higher order functions

* Reuse -> conciseness -  expressive power - polymorphism is about reuse - you can abstract out the common parts in processing multiple related but slightly different types
* Composition -> reuse
* typeclass hierarchy

Polymorphic Entities
--------------------

+-------------------+-----------------------------+
| Polymorphic types | Polymorphic functions       |
+-------------------+-----------------------------+

+-------------------------------------------------------------------+
| Polymorphism ~ Abstraction                                        |
+===================================================================+
| Common part                                                       |
+-------------------------------------------------------------------+
| Abstract or variable part representing multiple concrete entities |
+-------------------------------------------------------------------+

Polymorphism in Haskell
-----------------------

+-------------------------------------------------+
| Kind Polymorphism                               |
+-------------------------------------------------+

Examples:

+-----------------------------------------------------------------------------+
| Data Type Polymorphism                                                      |
+------------+-----------------------+----------------------------------------+
| Parametric | Polymorphic types     | Generic constructor instances for      |
|            |                       | the whole family                       |
+------------+-----------------------+----------------------------------------+
| Ad-hoc     | Data families         | Specialized constructor or type        |
|            +-----------------------+ instances for each member type of the  |
|            | Type synonym families | family                                 |
+------------+-----------------------+----------------------------------------+

Examples
~~~~~~~~

+-------------------------------------+---------------------------------------+
| Polymorphic types                   | Polymorphic Functions                 |
+-------------------------------------+---------------------------------------+
| a :: *                              | id :: a -> a                          |
+-------------------------------------+---------------------------------------+
| List a :: *                         | (++) :: List a -> List a -> List a    |
+-------------------------------------+---------------------------------------+

+-----------------------------------------------------------------------------+
| Function Polymorphism                                                       |
+------------+-------------+--------------------------------------------------+
| Parametric | Polymorphic | A single generic function instance with          |
|            | Functions   | parametrically polymorphic values as arguments.  |
+------------+-------------+--------------------------------------------------+
| Ad-hoc     | Typeclasses | Specialized function instances associated with   |
|            |             | each type which is a member of the typeclass.    |
+------------+-------------+--------------------------------------------------+

Polymorphic functions operate on polymorphic data. A polymorphic function has
at least one type variable in its type. Intuitively they operate on the common
part of the objects passed to it or can use polymorphic operations on the
abstract part. They can operate on the abstract type if the type is known to
belong to a typeclass via a typeclass constraint.

+-----------------------------------------------------------------------------+
| Polymorphic Functions                                                       |
+--------+--------------------------------------------------------------------+
| Rank   | Parametric type argument variables (values or functions)           |
+========+====================================================================+
| Rank-1 | monomorphic                                                        |
+--------+--------------------------------------------------------------------+
| Rank-2 | rank-1 polymorphic or lower                                        |
+--------+--------------------------------------------------------------------+
| Rank-n | rank n-1 polymorphic or lower                                      |
+--------+--------------------------------------------------------------------+

In contrast to a parametrically polymorphic data type, each member of a type
family is deconstructed differently therefore a generic polymorphic function
cannot be used on a type family.  So type families are naturally associated
with typeclasses where we write a type instance as well as a function instance
to handle that type instance.

Examples:

+-----------------------------------------------------------------------------+
| Typeclasses                                                                 |
+------------------+----------------------------------------------------------+
| Using parametric | Typeclass functions work on types which are              |
| types            | parameterized by the typeclass member                    |
|                  | type(s).                                                 |
+------------------+----------------------------------------------------------+
| Using associated | Typeclass instance creates an instance of a type family  |
| type family      | associated with the type class. Typeclass functions work |
|                  | on this particular instance of the data type.            |
+------------------+                                                          |
| Using associated |                                                          |
| data family      |                                                          |
+------------------+----------------------------------------------------------+

TBD: picture depicting typeclass member type -> polymorphic function
TBD: picture depicting typeclass member type -> associated family ->
polymorphic function

The data types that the typeclass functions work on can of course be
polymorphic (parametric or type family). Therefore these functions are
potentially polymorphic functions.

Polymorphic functions vs Typeclass
----------------------------------

If the logic for all types can be abstracted in such a way that a single
function body parameterized by the type can be used to represent the logic then
you can use a polymorphic function. Usually logic that works on a single
parameterized type.

When different types or types of different shapes need to support an abstract
function then we have to create a typeclass with the common functions and make
each type a member of that type class. The types may be pretty much unrelated
types with different shapes.

When the types that need to support the same abstract functions are related via
a type family then we can associate the type family with the typeclass and
instantiate the family members in each typeclass instance.

Examples:
