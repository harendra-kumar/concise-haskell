.. raw:: html

  <style> .red {color:red} </style>
  <style> .blk {color:black} </style>
  <style> .center { text-align: center;} </style>
  <style> .strike { text-decoration: line-through;} </style>

.. role:: strike
.. role:: center

.. role:: red
.. role:: blk

Type-o-pedia
============

Basics
------

Terminology
~~~~~~~~~~~

+----------------------------+-----------------------------------------------------------------+
| Type                       | Denotes rules that a value should conform to                    |
|                            | (e.g. Int or String)                                            |
+----------------------------+-----------------------------------------------------------------+
| Kind                       | Type of types (e.g. a type could be lifted or unlifted)         |
+----------------------------+-----------------------------------------------------------------+
| Rigid type                 | Type is fixed by annotation (signature) and not determined by   |
|                            | inference.                                                      |
+----------------------------+-----------------------------------------------------------------+
| Rigid type variable        | The scope (quantification) of the type variable is fixed by     |
| (skolem)                   | annotation.                                                     |
+----------------------------+-----------------------------------------------------------------+
| Unify                      | A type variable can be unified with other type variable if they |
|                            | are in the same quantification scope and therefore mean one and |
|                            | the same thing.                                                 |
+----------------------------+-----------------------------------------------------------------+
| Concrete                   | Represents a real physical value (not abstract)                 |
+----------------------------+-----------------------------------------------------------------+
| Monomorphic                | Has only one possible concrete representation                   |
+----------------------------+-----------------------------------------------------------------+
| Polymorphic                | Has multiple concrete representations (abstract, not concrete)  |
+----------------------------+-----------------------------------------------------------------+
| Monotype                   | A monomorphic type                                              |
+----------------------------+-----------------------------------------------------------------+
| Unboxed                    | Bare physical representation, no wrapping or indirection layer  |
+----------------------------+-----------------------------------------------------------------+
| Boxed                      | Physical representation wrapped with control info               |
|                            | (trackable heap object).                                        |
+----------------------------+-----------------------------------------------------------------+
| Lazy value                 | All ordinary Haskell values are lazy meaning they are evaluated |
|                            | on demand. Lazy values are also called lifted values.           |
+----------------------------+-----------------------------------------------------------------+
| Diverging computation      | A function or computation that does not return to the caller is |
|                            | said to diverge. Divergence is denoted by bottom.               |
+----------------------------+-----------------------------------------------------------------+
|                            | In order theory, the least element (if it exists) of a          |
|                            | partially ordered set is also called the bottom and is denoted  |
|                            | by ⊥.                                                           |
|                            +-----------------------------------------------------------------+
| Bottom (_|_)               | In Haskell, bottom is the least defined value added to all      |
|                            | types to denote, undefined, diverging and lazy values.          |
+----------------------------+-----------------------------------------------------------------+
| Lifting                    | Adding a `bottom` to an ordered set is called `lifting`.        |
+----------------------------+-----------------------------------------------------------------+
| Unlifted (type)            | Operational: Cannot be constructed lazily                       |
|                            +-----------------------------------------------------------------+
|                            | Denotational: Does not contain a bottom value                   |
+----------------------------+-----------------------------------------------------------------+
| Lifted (type)              | Operational: Can be constructed lazily                          |
|                            +-----------------------------------------------------------------+
|                            | Denotational: Contains a bottom value                           |
+----------------------------+-----------------------------------------------------------------+
| Primitives                 | Types which cannot be expressed in Haskell                      |
+----------------------------+-----------------------------------------------------------------+
| Primops                    | Functions operating on primitive types                          |
+----------------------------+-----------------------------------------------------------------+
| Open                       | Open to extension, can be extended (e.g. open type families)    |
+----------------------------+-----------------------------------------------------------------+
| Closed                     | Closed to extension, cannot be extended                         |
|                            | (e.g. closed type families)                                     |
+----------------------------+-----------------------------------------------------------------+

Lifting Types with Bottom
-------------------------

In theoretical terms adding a bottom value to the set of values denoted by a
type is called lifting the type.

A bottom value is implicit in lifted types. For example, you can imagine:

``data () = ()`` <=> ``data () = () | ⊥``

Bottom concept is useful in reasoning about some semantic properties of Haskell
programs like lazy evaluation, non-termination, partial functions, errors and
exceptions.

From an operational standpoint it means that the data constructors of a lifted
type are lazily evaluated. In the context of lazy evaluation (graph reduction)
the unevaluated expression can be thought of as bottom which can be evaluated
on demand to determine the actual value.

+-----------------------------------------------------------------------------+
| Any value of polymorphic type `forall a. a` denotes bottom. Functions       |
| denoting bottom can be used anywhere in an expression of any type.          |
+-----------------------------------------------------------------------------+
| Evaluating bottom (excluding lazy values) always results in an error or     |
| non-termination.                                                            |
+-----------+------+----------------------------------------------------------+
| error     | `::` | forall a. [Char] -> a                                    |
+-----------+------+----------------------------------------------------------+
| undefined | `::` | forall a. a                                              |
+-----------+------+----------------------------------------------------------+
| throw     | `::` | e -> a                                                   |
+-----------+------+-------------------------+--------------------------------+
| non-termination                            | let x = x                      |
+--------------------------------------------+--------------------------------+
| partial functions                          | head []                        |
+--------------------------------------------+--------------------------------+

Kinds
-----

Runtime Representation
~~~~~~~~~~~~~~~~~~~~~~

The runtime representation of lifted types is always boxed. Lifting is
represented by a closure at runtime. A closure can represent an undefined value
or bottom which can be refined by lazy evaluation.

Unlifted types always have a direct unboxed runtime representation.

Kinds of Concrete Types
~~~~~~~~~~~~~~~~~~~~~~~

+-----------+------+-------------------+-------------+-----------------------+
| Type      |      | Kind              | Runtime Rep | Remarks               |
+===========+======+===================+=============+=======================+
| .. class:: center                                                          |
|                                                                            |
| Unlifted Types                                                             |
+-----------+------+-------------------+-------------+-----------------------+
| Int#      | `::` | TYPE 'IntRep'     | Unboxed     | Direct Reg or Mem     |
+-----------+------+-------------------+-------------+-----------------------+
| Double#   | `::` | TYPE 'DoubleRep'  | Unboxed     | Direct Reg or Mem     |
+-----------+------+-------------------+-------------+-----------------------+
| Array#    | `::` | TYPE 'ArrayRep'   | Unboxed     | Indirect Heap Pointer |
+-----------+------+-------------------+-------------+-----------------------+
| .. class:: center                                                          |
|                                                                            |
| Lifted Types                                                               |
+-----------+------+-------------------+-------------+-----------------------+
| RealWorld | `::` | Type              | NA          | Compile time only     |
+-----------+------+-------------------+-------------+-----------------------+
| Int       | `::` | Type              | Boxed       |                       |
+-----------+------+-------------------+-------------+-----------------------+
| Maybe Int | `::` | Type              | Boxed       |                       |
+-----------+------+-------------------+-------------+-----------------------+

Kinds of Polymorphic Types (Type functions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

+-----------------------+-------------+--------------------------------------+
| Function              | Application | Failure Reason                       |
+-----------------------+-------------+--------------------------------------+
| Maybe :: Type -> Type | Maybe Int#  | Wrong kind ``TYPE 'IntRep'``         |
|                       |             | expected ``Type``                    |
+-----------------------+-------------+--------------------------------------+

.. _Primitive Types: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.0.0/GHC-Prim.html>

`Primitive Types`_
------------------

Unlifted Primitives
~~~~~~~~~~~~~~~~~~~

+-----------------+------------------------------------------------+
| Convention      | # postfix on unlifted primitives               |
+-----------------+------------------------------------------------+
| -XMagicHash     | Allow # as a postfix to identifiers & literals |
+-----------------+------------------------------------------------+
| -XUnboxedTuples | Allow unboxed tuples ``(# e_1, ..., e_n #)``   |
+-----------------+------------------------------------------------+

+---------+---------------------+--------------+-----------------+
| Basic   | Arrays              | Pointers etc | Concurrency etc |
+=========+=====================+==============+=================+
| Char#   | Array#              | Addr#        | MutVar#         |
+---------+---------------------+--------------+-----------------+
| Int#    | MutableArray#       | Weak#        | TVar#           |
+---------+---------------------+--------------+-----------------+
| Word#   | SmallArray#         | StablePtr#   | MVar#           |
+---------+---------------------+--------------+-----------------+
| Double# | ByteArray#          | StableName#  | ThreadId#       |
+---------+---------------------+--------------+-----------------+
| Float#  | ArrayArray#         | State#       | BCO#            |
+---------+---------------------+--------------+-----------------+
|         | MutableArrayArray#  | Proxy#       | `SIMD Vectors`  |
+---------+---------------------+--------------+-----------------+

Lifted Primitives
~~~~~~~~~~~~~~~~~

+------------+
| RealWorld  |
+------------+

Using Primitives
~~~~~~~~~~~~~~~~

+---------------------+--------------------------------+
| Importing           | Restrictions                   |
+=====================+================================+
| ``import GHC.Exts`` | Cannot use unlifted types in:  |
|                     | `newtype` definition           |
|                     | , top-level binding            |
|                     | , recursive binding            |
|                     | , lazy pattern match           |
+---------------------+--------------------------------+

Basic Haskell Types
-------------------

+-----------------------------------------------------------------------------------------------------+
| Data types defined using primitives                                                                 |
+---------------+---+----------------------------------------------------+----------------------------+
| data Char     | = | C# Char#                                           |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| data Int      | = | I# Int#                                            |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| data Word     | = | W# Word#                                           |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| data Float    | = | F# Float#                                          |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| data Double   | = | D# Double#                                         |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| newtype IO a  | = | IO (State# RealWorld -> (# State# RealWorld, a #)) | IO action                  |
+---------------+---+----------------------------------------------------+----------------------------+

+---------------+---+----------------------------------------------------+----------------------------+
| data ()       | = | ()                                                 | The void or unit datatype, |
|               |   |                                                    | 0-tuple                    |
+---------------+---+----------------------------------------------------+----------------------------+
| data (a, b)   | = | (a, b)                                             | 2-tuple                    |
+---------------+---+----------------------------------------------------+----------------------------+
| data [] a     | = | [] | a : [a]                                       | Lists                      |
+---------------+---+----------------------------------------------------+----------------------------+
| data Ordering | = | LT | EQ | GT                                       |                            |
+---------------+---+----------------------------------------------------+----------------------------+
| data Bool     | = | False | True                                       |                            |
+---------------+---+----------------------------------------------------+----------------------------+

Algebraic Data Construction
---------------------------

Basic Syntax
~~~~~~~~~~~~

+--------------------------------------------------------------------------------------------------------------------------+
| Bind a type to data constructor signatures                                                                               |
+------------------------------------------------+-----+-------------------------------------------------------------------+
| .. class:: center                              |     | .. class:: center                                                 |
|                                                |     |                                                                   |
| Type Level Function                            |     | Data Constructor Templates                                        |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
|         | Type Constructor    |      Parameter |     | Data Constructor    |       | Data Constructor                    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| data    | :red:`L`:blk:`ist`  | `a`            |  =  | :red:`E`:blk:`mpty` | ``|`` | :red:`C`:blk:`ons`  a   (List a)    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| where parameter `a` as well as all argument types of data constructors must be a concrete type of kind ``Type``          |
+--------------------------------------------------------------------------------------------------------------------------+

Type Constructor
................

TODO: Have separate example for concrete type. Differentiate type constructor
from a concrete type. type constructor is a function.

+-----------------------------------------------------------------------------------------+
| A concrete type or type function to instantiate a new type                              |
+----------------------+--------+------------------+--------------------------------------+
| Type                 |        | Kind             | Description                          |
+----------------------+--------+------------------+--------------------------------------+
| List                 | ``::`` | ``Type -> Type`` | Polymorphic type or type constructor |
+----------------------+--------+------------------+--------------------------------------+
| .. class:: center                                                                       |
|                                                                                         |
| Instances                                                                               |
+----------------------+--------+------------------+--------------------------------------+
| List Int             | ``::`` | ``Type``         | Concrete type (list of Ints)         |
+----------------------+--------+------------------+--------------------------------------+
| List (Maybe Int)     | ``::`` | ``Type``         | Concrete type (list of Maybe Ints)   |
+----------------------+--------+------------------+--------------------------------------+
| :strike:`List Maybe` |        |                  | Kind mismatch                        |
+----------------------+--------+------------------+--------------------------------------+

Data Constructors
.................

+--------------------------------------------------------------------------------------------------------+
| Return a data of a certain type by `creating` it or by `composing` argument values into a new data.    |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Data Constructor  |        | Type                          | Description                               |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Empty             | ``::`` | List a                        | Create a new value (empty list)           |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Cons              | ``::`` | Cons :: a -> List a -> List a | Compose two values (`a` and `List a`)     |
+-------------------+--------+-------------------------------+-------------------------------------------+

Data Construction Example
~~~~~~~~~~~~~~~~~~~~~~~~~

::

    l1, l2, l3 :: List Char

    l3 = Cons 'b' l2     l2 = Cons 'a' l1     l1 = Empty

    +--------+-------+      +--------+-------+      +--------+
    |  'b'   |  l2   |----->|  'a'   |  l1   |----->|  Empty |
    +--------+-------+      +--------+-------+      +--------+

Terminology
...........

+-----------+---------------------------------------+-------------+
| Sum       | data Bool = False | True              | Monomorphic |
+-----------+---------------------------------------+-------------+
| Product   | data Point = Point Int Int            | Monomorphic |
+-----------+---------------------------------------+-------------+
| Recursive | data List a = Empty | Cons a (List a) | Polymorphic |
+-----------+---------------------------------------+-------------+

Generalized Algebraic Data Type (GADT) Syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------------+
| Standard algebraic data type syntax                              |
|                                                                  |
| * Each data constructor has the same return type which is        |
|   implicit and the same as the data type.                        |
| * The data type parameter scopes over the constructors and is    |
|   used as a type parameter in the constructors.                  |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a = Empty | Cons a (List a)                           |
+------------------------------------------------------------------+
| Generalized (GADT) Syntax                                        |
|                                                                  |
| * GADT syntax essentially specifies two things, the arity of the |
|   type constructor and signatures of all data constructors       |
|   explicitly.                                                    |
| * It allows the return type of each data constructor to be       |
|   different.                                                     |
| * The data type parameter is only a placeholder and has no scope.|
|   It indicates only the arity of the type function.              |
| * Type variables across different constructors are not related.  |
| * Type variables featuring in the return type of a constructor   |
|   are implicitly universally quantified.                         |
| * Type variables not featuring in the return type of a           |
|   constructor are implicitly existentially quantified            |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a where     -- 'a' has no scope, only a placeholder   |
|    Empty :: List b                                               |
|    Cons  :: c -> List c -> List c                                |
+------------------------------------------------------------------+
| The type of a specific instance must match the return type of a  |
| constructor which in turn determines the actual signature of the |
| constructor. For example, when used as ``List Int``              |
| the constructors in the example above will read as:              |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|    Empty :: List Int                     -- b ~ Int              |
|    Cons  :: Int -> List Int -> List Int  -- c ~ Int              |
+------------------------------------------------------------------+
| Just like in function signatures, multiple constructors with the |
| same signature can be grouped together.                          |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data T x where                                                  |
|   T1,T2 :: b -> T b                                              |
|   T3 :: T a                                                      |
+------------------------------------------------------------------+
| Since type parameters only determine the arity we can            |
| omit them and use the kind instead. ``Bar a b`` in               |
| ``data Bar a b where ...`` can also be written as:               |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  1) Bar :: Type -> Type -> Type                                  |
|  2) Bar a :: (Type -> Type)                                      |
|  3) Bar a (b :: Type -> Type)                                    |
+------------------------------------------------------------------+

Typeclass Derivation and Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------+-------------------------------------------------------+
| Haskell98 Syntax                                           | GADT Syntax                                           |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Derivation                                                                                               |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|  data Maybe a = Nothing | Just a                           |    data Maybe a where                                 |
|      deriving (Eq, Ord)                                    |        Nothing :: Maybe a                             |
|                                                            |        Just    :: a -> Maybe a                        |
|                                                            |        deriving (Eq, Ord)                             |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Constraint                                                                                               |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|  data Set a = Eq a => MkSet [a]                            |   data Set a where                                    |
|                                                            |     MkSet :: Eq a => [a] -> Set a                     |
+------------------------------------------------------------+-------------------------------------------------------+
| * Construction `requires` ``Eq a``: makeSet :: :red:`Eq a =>` [a] -> Set a; makeSet xs = MkSet (nub xs)            |
| * Pattern match `provides` ``Eq a``: insert a (MkSet as) | a :red:`\`elem\`` as = MkSet as                         |
| * Note: Haskell98 `requires` instead of `providing` ``Eq a`` in pattern match.                                     |
+--------------------------------------------------------------------------------------------------------------------+

Misc Data Construction Syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------------------------------------------------------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| All data constructors are lazy by default. You can add strictness annotations to make them strict.                 |
+------------------------------------------------------------+-------------------------------------------------------+
|                                                            | ::                                                    |
|                                                            |                                                       |
|                                                            |   data Term a where                                   |
|                                                            |     Lit :: !Int -> Term Int                           |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| Infix type constructor                                                                                             |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         |                                                       |
|                                                            |                                                       |
|  ``data a :*: b = Foo a b``                                |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XTypeOperators                                                                                                    |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         |                                                       |
|                                                            |                                                       |
|  data a + b = Plus a b                                     |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XEmptyDataDecls                                                                                                   |
+--------------------------------------------------------------------------------------------------------------------+
| ::                                                                                                                 |
|                                                                                                                    |
|  data T a    -- T :: Type -> Type                                                                                  |
+--------------------------------------------------------------------------------------------------------------------+

Records
~~~~~~~

+-----------------------------------------------------------------------------+
| `-XNoTraditionalRecordSyntax` (7.4.1) -- to disable the record syntax       |
+=============================================================================+
| .. class :: center                                                          |
|                                                                             |
| Records                                                                     |
+----------------------+------------------------------------------------------+
| ::                   | ::                                                   |
|                      |                                                      |
|  data R =            |   data R where                                       |
|    R {               |     R :: {                                           |
|        x :: String   |         x  :: String                                 |
|      , y :: Int      |       , y  :: Int                                    |
|    } deriving (Show) |       } -> R                                         |
|                      |     deriving (Show)                                  |
+----------------------+------------------------------------------------------+
| Selector functions to extract a field from a record data structure are      |
| automatically generated for each record field::                             |
|                                                                             |
|  x :: R -> String                                                           |
|  y :: R -> Int                                                              |
+-----------------------------------------------------------------------------+
| `-XDuplicateRecordFields` (8.0.1) allows using identical fields in different|
| records even in the same module. Selector functions and updates are         |
| disambiguated using the type of the field.                                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data S =                                                                   |
|    S {                                                                      |
|        x :: String                                                          |
|      , z :: Int                                                             |
|    } deriving (Show)                                                        |
+-----------------------------------------------------------------------------+
| Exporting and importing selector functions:                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  Module M (y)    where ...     -- only when y is unambiguous field          |
|  Module M (R(x)) where ...     -- even when x is ambiguous field            |
|                                                                             |
|  import M (y)                  -- only when y is unambiguous field          |
|  import M (R(x))               -- even when x is ambiguous field            |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Construction and pattern matching                                           |
+=============================================================================+
| Record constructor brackets {} have a higher precedence than function       |
| application.                                                                |
+-----------------------------------------------------------------------------+
| `-XDisambiguateRecordFields` allows using record fields x and y unqualified |
| even if they clash with field names in other records and even when the      |
| record is defined in a module which is imported qualified.                  |
+-----------------------------------------------------------------------------+
| **Construction**                                                            |
+----------------------------+------------------------------------------------+
| ``show (R "a" 1)``         | ``show R { y = 1, x = "a" }                    |
|                            | -- Note precedence of {}``                     |
+----------------------------+------------------------------------------------+
| ``r = R "a" 1``            | ``r = R { y = 1, x = "a" }``                   |
+----------------------------+------------------------------------------------+
| `-XRecordWildCards`        | ``let {x = "a"; y = 2} in R {..}               |
|                            | -- R {x = x, y = y}``                          |
+----------------------------+------------------------------------------------+
| **Pattern matching**                                                        |
+----------------------------+------------------------------------------------+
| ``f (R _ _)   = ...``      | ``f R {}                 = ...                 |
|                            | -- Note precedence of {}``                     |
+----------------------------+------------------------------------------------+
| ``f (R "a" 1) = ...``      | ``f R {x = "a", y = 1}   = ...``               |
+----------------------------+------------------------------------------------+
| ``f (R a b) = ...``        | ``f (R {x = a, y = b})   = a ++ show b``       |
+----------------------------+------------------------------------------------+
| `-XNamedFieldPuns`         | ``f (R {x, y})           = ...                 |
|                            | -- f (R {x = x, y = y})``                      |
|                            +------------------------------------------------+
|                            | ``f (R {x, y = b})       = ...                 |
|                            | -- f (R {x = x, y = b})``                      |
|                            +------------------------------------------------+
|                            | ``f (R {M.x, M.y})       = ... -- M is module  |
|                            | qualifier``                                    |
+----------------------------+------------------------------------------------+
| `-XRecordWildCards`        | ``f (R {..})             = ...                 |
|                            | -- f (R {x = x, y = y})``                      |
| ``..`` expands to missing  +------------------------------------------------+
| `in-scope` record fields   | ``f (R {x = "a", ..})    = ...                 |
|                            | -- f (R {x = "a", y = y})``                    |
|                            +------------------------------------------------+
|                            | ``import R(y)``                                |
|                            |                                                |
|                            | ``f (R {..})             = ...                 |
|                            | -- f (R {y = y})``                             |
+----------------------------+------------------------------------------------+

+-----------------------------------------------------------------------------+
| Access and update                                                           |
+=============================================================================+
| **Accessing field 'x' using its selector function**                         |
+----------------------------------+------------------------------------------+
| ``x R {x = "a", y = 1}``         | ``x r``                                  |
+----------------------------------+------------------------------------------+
| When using `-XDuplicateRecordFields` disambiguate selectors:                |
+-----------------------------------------------------------------------------+
| By inferred or explicit type of the selector function (e.g. ``x``).         |
+-----------------------+-------------------+---------------------------------+
| ``v = x :: S -> Int`` | ``v :: S -> Int`` | ``f :: (S -> Int) -> _``        |
|                       |                   |                                 |
|                       | ``v = x``         | ``f x``                         |
+-----------------------+-------------------+---------------------------------+
| By explicit but not inferred type of the record being accessed (e.g. ``s``).|
+-----------------------+-----------------------------------------------------+
| ``ok s = x (s :: S)`` | ``bad :: S -> Int``                                 |
|                       |                                                     |
|                       | ``bad s = x s        -- Ambiguous``                 |
+-----------------------+-----------------------------------------------------+
| If only one of the conflicting selectors is imported by a module then it    |
| can be used unambiguously.                                                  |
+-----------------------------------------------------------------------------+
| **Updating one or more fields**                                             |
+----------------------------------+------------------------------------------+
| ``R {x = "a", y = 1} {x = "b"}`` | ``r { x = "b", y = 2}``                  |
+----------------------------------+------------------------------------------+
| When using `-XDuplicateRecordFields`, disambiguate duplicate fields:        |
+-----------------------------------------------------------------------------+
| By field names:                                                             |
+-----------------------------------------------------------------------------+
| ``s {z = 5} -- field z occurs only in record type S``                       |
+-----------------------------------------------------------------------------+
| By the inferred or explicit type of the update application                  |
| (e.g. ``s {x = 5}``).                                                       |
+------------------------+-------------------+--------------------------------+
| ``v = s {x = 5} :: S`` | ``v :: S -> S``   | ``f :: S -> _``                |
|                        |                   |                                |
|                        | ``v = s {x = 5}`` | ``f (s {x = 5})``              |
+------------------------+-------------------+--------------------------------+
| By the explicit but not inferred type of the record being updated           |
| (e.g. ``s``).                                                               |
+-----------------------------+-----------------------------------------------+
| ``ok s = (s :: S) {x = 5}`` | ``bad :: S``                                  |
|                             |                                               |
|                             | ``bad s = s {x = 5} -- Ambiguous``            |
+-----------------------------+-----------------------------------------------+

Existential Quantification
~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------------------------------------------------------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XExistentialQuantification                                                                                        |
+--------------------------------------------------------------------------------------------------------------------+
| Quantified type variables that appear in arguments but not in the result type for any constructor are              |
| `existentials`. The existence, visibility or scope of these type variables is localized to the given constructor.  |
| They will typecheck with other instances only within this local scope.                                             |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|   data Foo = forall a.                                     |   data Foo where                                      |
|     Show a => Foo a (a -> a)                               |     Foo :: Show a => a -> (a -> a) -> Foo             |
|                                                            |                                                       |
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|   data Counter a = forall self.                            |   data Counter a where                                |
|     Show self => NewCounter                                |     NewCounter :: Show self =>                        |
|     { _this    :: self                                     |     { _this    :: self                                |
|     , _inc     :: self -> self                             |     , _inc     :: self -> self                        |
|     , _display :: self -> IO ()                            |     , _display :: self -> IO ()                       |
|     , tag      :: a                                        |     , tag      :: a                                   |
|     }                                                      |     } -> Counter a                                    |
+------------------------------------------------------------+-------------------------------------------------------+
| The type of an existential variable is fixed during construction based on the type used in the constructor call.   |
+--------------------------------------------------------------------------------------------------------------------+
| Existentials can be extracted by pattern match but only in `case` or `function definition` and not in `let` or     |
| `where` bindings.                                                                                                  |
+--------------------------------------------------------------------------------------------------------------------+
| The extracted value can be consumed by any functions in the scope of the existential.                              |
| The typeclass constraint when specified, is available as usual on pattern match. You can use the existential       |
| type's typeclass functions on it: ``f NewCounter {_this, _inc} = show (_inc _this)``                               |
+--------------------------------------------------------------------------------------------------------------------+
| Record fields using existentials are `private`. They will not get a selector function and cannot be updated. For   |
| example, all fields prefixed with ``_`` in the above example are private.                                          |
+--------------------------------------------------------------------------------------------------------------------+

GADT (Aggregated Type)
~~~~~~~~~~~~~~~~~~~~~~

* http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf Fun with phantom
  types.

+-----------------------------------------------------------------------------+
| -XGADTs                                                                     |
+-----------------------------------------------------------------------------+

+--------------------------------------------------------------------------------+
| Representing terms in an expression with static typechecking.                  |
+--------------------------------------------------------------------------------+
| The type of an evaluated expression depends on the specific expression         |
| being evaluated.                                                               |
+--------------------------------------------------------------------------------+
| ::                                                                             |
|                                                                                |
|    eval (Lit 10)                                                 -- Int        |
|    eval (Succ (Lit 10))                                          -- Int        |
|    eval (IsZero (Lit 10))                                        -- Bool       |
|    eval (If (IsZero (Lit 10)) (Lit 0) (Lit 1))                   -- Int        |
|    eval (If (IsZero (Lit 10)) (IsZero (Lit 0)) (IsZero (Lit 1))) -- Bool       |
|    eval (Pair (Lit 10) (Lit 20))                                 -- (Int, Int) |
+--------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| An expression is represented by a data type which is a collection of terms  |
| in that expression.                                                         |
+-----------------------------------------------------------------------------+
| Since each expression evaluates to a different type `we need what that type |
| is for each expression`. `We also need a way to somehow propagate this type |
| information and use it when we evaluate the expression`.                    |
+-----------------------------------------------------------------------------+
| The type information for each expression is encoded as the return type of   |
| the constructor e.g. ``Term Bool`` return type means the expression         |
| evaluates to a ``Bool`` value.                                              |
+-----------------------------------------------------------------------------+
| The type ``Term a`` represents any term i.e. an abstraction for the         |
| aggregation of the return types of all constructors of this data type.      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   data Term a where                                                         |
|     Lit    :: Int -> Term Int                                               |
|     Succ   :: Term Int -> Term Int                                          |
|     IsZero :: Term Int -> Term Bool                                         |
|     If     :: Term Bool -> Term a -> Term a -> Term a                       |
|     Pair   :: Term a -> Term b -> Term (a,b)                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| ``Term a`` admits all constructors types of ``Term``.                       |
| ``a`` the return type of ``eval``, depends on the specific constructor      |
| being evaluated e.g. when we evaluate ``Lit`` we know from the GADT         |
| definition that ``Lit``'s type is ``Term Int`` so ``a`` must be ``Int``.    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Explicit signature is required                                          |
|  -- The return type 'a' of this function is dependent                       |
|  -- on the type of the constructor passed to it                             |
|  eval :: Term a -> a                                                        |
|                                                                             |
|  eval (Lit i)      = i                                   -- a ~ Int         |
|  eval (Succ t)     = 1 + eval t                          -- a ~ Int         |
|  eval (IsZero t)   = eval t == 0                         -- a ~ Bool        |
|  eval (If b e1 e2) = if eval b then eval e1 else eval e2 -- a ~ a           |
|  eval (Pair e1 e2) = (eval e1, eval e2)                  -- a ~ (a1, a2)    |
+-----------------------------------------------------------------------------+
| In other words a pattern matching instance retrieves the type               |
| encoded in the constructor return type to determine `a`. The retrieved type |
| can then be used to write type specific code with proper typechecking.      |
+-----------------------------------------------------------------------------+
| The concept inherently requires an explicit type signature in a pattern     |
| match for the following:                                                    |
|                                                                             |
| * scrutinee                                                                 |
| * entire case expression                                                    |
| * free variables mentioned in any of the case alternatives                  |
+-----------------------------------------------------------------------------+
| `deriving` clause cannot be used                                            |
+-----------------------------------------------------------------------------+

+------------------------------------------------------------------------------------------------------+
| A polymorphic type and an aggregated type (GADT) are two opposite concepts.                          |
+-------------------------------------------------+----------------------------------------------------+
| A polymorphic type                              | Aggregated type (GADT)                             |
+-------------------------------------------------+----------------------------------------------------+
| All constructors return the same type           | One or more constructors return a concrete type    |
| parameterized by a type variable.               | instance (e.g. Term Int).                          |
+-------------------------------------------------+----------------------------------------------------+
| Defines an asbtract type e.g. ``List``.         | Defines the sum type as a group of concrete type   |
|                                                 | instances.                                         |
+-------------------------------------------------+----------------------------------------------------+
| We `instantiate` ``List`` to create concrete    | We `abstract` the group of concrete types          |
| type instances.                                 | to ``Term a``.                                     |
+------------------------+------------------------+------------------------+---------------------------+
| Define Abstract Type   | Create Instances       | Define instances       | Create Abstraction        |
+------------------------+------------------------+------------------------+---------------------------+
| List a                 | List Int               | Term Int               | Term a                    |
|                        +------------------------+------------------------+                           |
|                        | List Bool              | Term Bool              |                           |
|                        +------------------------+------------------------+                           |
|                        | List (Int, Bool)       | Term (a,b)             |                           |
+------------------------+------------------------+------------------------+---------------------------+
| A type signature specifies a concrete type      | An explicit type signature specifies the abstract  |
| instance via explicit specification or          | type ``Term a``. The value of ``a`` is             |
| inference.                                      | supplied by the typechecker on pattern match.      |
+-------------------------------------------------+----------------------------------------------------+

+-----------------------------------------------------------------------------+
| Another way to think about it is to think of                                |
| `eval` as a polymorphic function representing a whole family of functions   |
| with `a` ranging over the return types of constructors of `Term`:           |
| ::                                                                          |
|                                                                             |
|  eval :: Term Int -> Int                                                    |
|  eval :: Term Bool -> Bool                                                  |
|  eval :: Term (Int, Bool) -> (Int, Bool)                                    |
|  eval :: Term (Bool, Int) -> (Bool, Int)                                    |
|                                                                             |
| The appropriate definition is chosen statically depending on the            |
| constructor passed to eval.                                                 |
| ::                                                                          |
|                                                                             |
|  eval :: Term Int -> Int                                                    |
|  eval (Lit i)      = i                                                      |
|                                                                             |
| Here the definitions for the recursive calls to eval will be chosen         |
| depending on the types of b, e1 and e2.                                     |
| ::                                                                          |
|                                                                             |
|  eval :: Term a -> a                                                        |
|  eval (If b e1 e2) = if eval b then eval e1 else eval e2                    |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Another example.                                                            |
+-----------------------------------------------------------------------------+
| Accepting a generic argument (``Int`` or ``Char``) to a function.           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Encode type information in constructor return types                     |
|  data Info a where                                                          |
|    InfoInt    :: Info Int  -- constructor encoding Int in return type       |
|    InfoChar   :: Info Char -- constructor encoding Char in return type      |
|                                                                             |
|  -- Signature with abstract type (Info a) MUST be supplied by programmer    |
|  -- Parameter 'a' is automatically determined by the typechecker            |
|  -- via pattern match on constructors                                       |
|  incr :: Info a -> a -> Int                                                 |
|  incr InfoInt  i    = i + 1         -- a ~ Int                              |
|  incr InfoChar c    = ord c + 1     -- a ~ Char                             |
|                                                                             |
|  -- Call the function with varying type argument but explicit type info     |
|  incr InfoInt 5                                                             |
|  incr InfoChar 'a'                                                          |
+-----------------------------------------------------------------------------+
| Its like constraint solving, the value of ``a`` gets computed by other      |
| available information rather than being supplied. Of course the type        |
| signature must be supplied with the unknowns at the right places.           |
+-----------------------------------------------------------------------------+

Deconstruction (Pattern Matching)
---------------------------------

* TBD define scrutinee

+-----------------------------------------------------------------------------+
| Pattern matching is the only way to destructure algebraic data              |
+-----------------------------------------------------------------------------+
| A concrete data structure is represented by one of multiple alternative     |
| constructors as we saw in data type definitons. Pattern matching is reverse |
| of the data type construction process i.e. an existing data structure's     |
| constructor is broken down into its components.                             |
|                                                                             |
| We write a constructor pattern on the LHS of an equation and the data       |
| structure to be decomposed on the RHS. A pattern looks like a constructor   |
| call except that the arguments are unbound variables. If the pattern        |
| matches with the data structure then the variables in the pattern are       |
| bound to the corresponding values of the data structure.                    |
+-----------------------------------------------------------------------------+
| Pattern match in `case` and `function definition` are always strict by      |
| default. In fact this is the only way to strictness in Haskell.             |
+-----------------------------------------------------------------------------+
| Pattern match in `let` and `where` clauses are always lazy by default       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Pattern match is lazy                                                   |
|  let Cons x xs = list                                                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Pattern match is lazy                                                   |
|  where Cons x xs = list                                                     |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Pattern match is strict                                                 |
|  case list of                                                               |
|    Cons x xs -> ...                                                         |
|    Empty     -> ...                                                         |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Pattern match is strict                                                 |
|   f (Cons x xs) = ...                                                       |
|   f (Empty)     = ...                                                       |
|                                                                             |
|   f list -- apply the function to a list                                    |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Lazy or irrefutable patterns - pattern match always succeeds.               |
+-----------------------------------------------------------------------------+
| Lazy patterns work well only when you have single constructor for the type, |
| e.g. tuples. For example `f undefined` will work on the following:          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   f ~(x,y) = 1    -- will not evaluate the tuple                            |
+-----------------------------------------------------------------------------+
| With multiple equations lazy match will always match the first one:         |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f ~(Just x) = 1                                                            |
|  f Nothing   = 2    -- will never match                                     |
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
|                                                                             |
+-----------------------------------------------------------------------------+

Pattern Synonyms
----------------

+-----------------------------------------------------------------------------+
| `-XPatternSynonyms` (7.8.1)                                                 |
+=============================================================================+
| A pattern synonym is a function that can be instantiated to a pattern or    |
| constructor                                                                 |
+---------------------+-------------------------------------------------------+
| Match only          | ``pattern HeadP x <- x : xs                           |
|                     | -- match the head of a list``                         |
+---------------------+-------------------------------------------------------+
| For `match and construct` pattern synonyms all the variables of the         |
| right-hand side must also occur on the left-hand side; also, wildcard       |
| patterns and view patterns are not allowed.                                 |
+---------------------+-------------------------------------------------------+
| Match and construct | ``pattern Singleton x  =  [x]                         |
| (Symmetric          | -- match or construct a singleton list``              |
| bidirectional)      |                                                       |
+---------------------+-------------------------------------------------------+
| Match and construct | ::                                                    |
| (Assymetric         |                                                       |
| bidirectional)      |  pattern Head x <- x:xs where   -- match              |
|                     |      Head x = [x]               -- construct          |
+---------------------+-------------------------------------------------------+
| Example                                                                     |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let list = Head "a"                                                       |
|   let Head x = [1..]                                                        |
+-----------------------------------------------------------------------------+
| A pattern synonym:                                                          |
|                                                                             |
| * starts with an uppercase letter just like a constructor.                  |
| * can be defined only at top level and not as a local definition.           |
| * can be defined as infix as well.                                          |
| * cannot be defined recursively.                                            |
+-----------------------------------------------------------------------------+
| Import and export                                                           |
+-----------------------------------------------------------------------------+
| Standalone                                                                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  module M (pattern Head) where ... -- export only the pattern               |
|  import M (pattern Head)           -- import only the pattern               |
|  import Data.Maybe (pattern Just)  -- import only data constructor 'Just'   |
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
| Types                                                                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  pattern P ::                                                               |
|            CReq          -- required to match the pattern                   |
|         => CProv         -- provided on pattern match                       |
|         => t1 -> t2 -> ... -> tN -> t                                       |
|  pattern P var1  var2  ...    varN <- pat                                   |
|                                                                             |
|  pattern P :: CReq => ...        -- CProv is omitted                        |
|  pattern P :: () => CProv => ... -- CReq is omitted                         |
|                                                                             |
|  Use of a bidirectional pattern synonym as an expression has the type:      |
|  (CReq, CProv) => t1 -> t2 -> ... -> tN -> t                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| A record pattern synonym behaves just like a record.                        |
| Does not seem to work before 8.0.1                                          |
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

Type Synonyms
-------------

+-----------------------------------------------------------------------------+
| A type synonym is a function that expands to a type                         |
+-----------------------------------------------------------------------------+
|  ``type ThisOrThat a b = Either a b``                                       |
|                                                                             |
|  ``type ThisOrInt  a   = Either a Int``                                     |
+-----------------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------------+
| Extended syntax                                                                                               |
+------------------------------------------------------------+--------------------------------------------------+
| type a :+: b = Either a b                                  | Infix type constructor                           |
+------------------------------------------------------------+--------------------------------------------------+
| type Foo = Int + Bool                                      | -XTypeOperators                                  |
+------------------------------------------------------------+--------------------------------------------------+
| type Discard a = forall b. Show b => a -> b -> (a, String) | -XLiberalTypeSynonyms                            |
+------------------------------------------------------------+--------------------------------------------------+
| type Pr = (# Int, Int #)                                   | -XLiberalTypeSynonyms (unboxed tuple)            |
+------------------------------------------------------------+--------------------------------------------------+
| f :: Foo (forall b. b->b)                                  | -XLiberalTypeSynonyms (forall)                   |
+------------------------------------------------------------+--------------------------------------------------+
| foo :: Generic Id []                                       | -XLiberalTypeSynonyms (partial application)      |
+------------------------------------------------------------+--------------------------------------------------+

newtype
-------

+-----------------------------------------------------------------------------+
| Wrap an existing type into a new type                                       |
+-----------------------------------------------------------------------------+
| newtype N = W (original type) deriving ...                                  |
+-----------------------------------------------------------------------------+
| `W is not a data constructor`, it does not construct data, it is just a type|
| level wrapper to wrap the original type into a new type N. Since W is not a |
| data constructor:                                                           |
|                                                                             |
| * you cannot provide multiple arguments to W. It only `wraps` a type, it    |
|   does not construct a type.                                                |
| * it does not lift the wrapped type, however it wraps only lifted types.    |
| * you can’t use existential quantification for newtype declarations.        |
| * it is just a type level artifiact and has no runtime overhead.            |
+-----------------------------------------------------------------------------+
| However just like data constructors, you can:                               |
|                                                                             |
| * pattern match on wrapper W to extract the original type                   |
| * use a deriving clause                                                     |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  newtype WrapInt = WrapInt Int                                              |
|  newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  |
+-----------------------------------------------------------------------------+
| Unlike a type synonym the type created by `newtype` is an entirely new type |
| and cannot be used in place of the original type.                           |
+-----------------------------------------------------------------------------+
| Newtypes may also be used to define recursive types. For example:           |
| ::                                                                          |
|                                                                             |
|  newtype List a = In (Maybe (a, List a))                                    |
+-----------------------------------------------------------------------------+

Data Families
~~~~~~~~~~~~~

+----------------------------------------------------------------------+
| A polymorphic type is a type function, it is a `total function`      |
| which defines the data constructors generically for all values of    |
| the type parameter.                                                  |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data List a = Empty | Cons a (List a)                               |
+----------------------------------------------------------------------+
| Examples of instances:                                               |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  List Char  -- Empty | Cons Char (List Char)                         |
|  List ()    -- Empty | Cons () (List ())                             |
+----------------------------------------------------------------------+

+----------------------------------------------------------------------+
| A data family is a type function, it is a `partial function` defined |
| only for the members of the family, each providing its own specific  |
| data constructor definitions. The function is open to extension as   |
| new instances can be defined later.                                  |
+----------------------------------------------------------------------+
| Prototype: declare the kind signature of the type function.          |
| All of the following declarations are equivalent:                    |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data family List a                                                  |
|  data family List a :: Type                                          |
|  data family List   :: Type -> Type                                  |
+----------------------------------------------------------------------+
| Instances: define the type function for specific values of the       |
| parameters (`a` in the above example) known as members of the family |
| (comparable to function definitions using pattern match)             |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data instance List Char = Empty | Cons Char (List Char)             |
|  data instance List ()   = Count Int                                 |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  newtype instance List ()   = Count Int                              |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data family G a b                                                   |
|  data instance G [a] b where        -- GADT                          |
|     G1 :: c -> G [Int] b                                             |
|     G2 :: G [a] Bool                                                 |
+----------------------------------------------------------------------+
| * The type function instance definition must match the kind          |
|   signature (i.e. arity) of the family                               |
| * The function cannot be defined more than once for the same value,  |
|   i.e. instance overlap is not allowed                               |
| * You can use a deriving clause on a data instance or newtype        |
|   instance declaration                                               |
|                                                                      |
| Type parameters of the function may not contain:                     |
|                                                                      |
| * forall types                                                       |
| * type synonym families                                              |
| * partially applied type synonyms                                    |
| * fully applied type synonyms expanding to inadmissible types        |
+----------------------------------------------------------------------+

Type Synonym Families
~~~~~~~~~~~~~~~~~~~~~

+-------------------------------------------------------------------------------------+
| Open families (open to extension by adding instances)                               |
+-------------------------------------------------------------------------------------+
| Declare the kind signature:                                                         |
+-------------------------------------------------------------------------------------+
| The number of parameters in a type family declaration, is the family’s              |
| arity. The kind of a type family is not sufficient to determine a family’s          |
| arity. So we cannot use just the kind signature in declaration like we can          |
| in data families.                                                                   |
+-------------------------------------------------------------------------------------+
| ::                                                                                  |
|                                                                                     |
|  type family F1 c                    -- Arity 1, F  :: Type -> Type                 |
|  type family F1 c    :: Type         -- Arity 1, F  :: Type -> Type                 |
|  type family F2 a b  :: Type -> Type -- Arity 2, F2 :: Type -> Type -> Type -> Type |
|  type family F3 a    :: k            -- Poly kinded, k is an implicit parameter     |
+-------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Define instances:                                                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type instance F1 [e] = e                                                   |
+-----------------------------------------------------------------------------+
| Instances may overlap but cannot have conflicting LHS and RHS across        |
| instance equations                                                          |
+----------------------------------+------------------------------------------+
| ::                               |                                          |
|                                  |                                          |
|  type instance F (a, Int) = [a]  | Compatible overlap, allowed.             |
|  type instance F (Int, b) = [b]  |                                          |
+----------------------------------+------------------------------------------+
| ::                               | Conflicting overlap, not allowed:        |
|                                  |                                          |
|  type instance G (a, Int)  = [a] | * (Char, Int) = [Char]                   |
|  type instance G (Char, a) = [a] | * (Char, Int) = [Int]                    |
+----------------------------------+------------------------------------------+
| ::                               |                                          |
|                                  |                                          |
|  type instance H x   x = Int     | Conflicting overlap when x is infinite   |
|  type instance H [x] x = Bool    | nesting of lists. Not allowed.           |
+----------------------------------+------------------------------------------+
| For a poly kinded family the kind variable is an implicit parameter.        |
+----------------------------------+------------------------------------------+
| ::                               | Ok, because they differ in the implicit  |
|                                  | kind parameter.                          |
|  type family J a :: k            |                                          |
|  type instance J Int = Bool      |                                          |
|  type instance J Int = Maybe     |                                          |
+----------------------------------+------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F a :: Type                                                    |
|  type instance F (F a)   = a            -- WRONG: family in parameter       |
|  type instance F (forall a. (a, b)) = b -- WRONG: forall in parameter       |
|  type instance F Float = forall a.a     -- WRONG: forall in RHS             |
+-----------------------------------------------------------------------------+
| Applications: must be fully saturated with respect to the family arity      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F a b :: Type -> Type                                          |
|  F Char [Int]                 -- OK!  Kind: Type -> Type                    |
|  F Char [Int] Bool            -- OK!  Kind: Type                            |
|  F IO Bool                    -- WRONG: kind mismatch for IO                |
|  F Bool                       -- WRONG: unsaturated application             |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Closed families (Closed to any further extension)                           |
+-----------------------------------------------------------------------------+
| Declared with a where clause, equations are tried in order,                 |
| from top to bottom                                                          |
+----------------------------------+------------------------------------------+
| ::                               |                                          |
|                                  |                                          |
|  type family F a where           | Incompatible equations                   |
|    F Int = Bool                  | F a does not simplify                    |
|    F a   = Char                  | F Double simplifies to Char              |
+----------------------------------+------------------------------------------+
| ::                               |                                          |
|                                  |                                          |
|  type family G a where           | Fully compatible equations               |
|    G Int = Int                   | G a simplifies to a                      |
|    G a   = a                     |                                          |
+----------------------------------+------------------------------------------+
| Creating an instance of a closed family will result in an error             |
+-----------------------------------------------------------------------------+

-XUndeciableInstances: allow undecidable type synonym instances.

Fun With Types
--------------

Specializing Polymorphic Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Smart Constructors
~~~~~~~~~~~~~~~~~~

* Type system is limited in expressing restrictions on types
* For example how do you represent a positive number less than 10?
* To overcome the limitation we wrap the type constructors in "smart
  constructors" which are nothing but functions with additional checks on the
  constructed value. The original type constructors are not exported so the
  only way to construct is via smart constructors which check additional rules.

For example::

    data LessThanTen = LTT Int
    mkLTT n = if n < 0 || n >= 10
      then error "Invalid value"
      else LTT n

Phantom Types
~~~~~~~~~~~~~

::

  data T = TI Int | TS String
  plus :: T -> T -> T
  concat :: T -> T -> T

  data T a = TI Int | TS String
  plus :: T Int -> T Int -> T Int
  concat :: T String -> T String -> T String

Dictionary Reification
~~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|  data NumInst a = Num a => MkNumInst                       |   data NumInst a where                                |
|                                                            |    MkNumInst :: Num a => NumInst a                    |
+------------------------------------------------------------+-------------------------------------------------------+
| We can pattern match on ``MkNumInst`` instead of using a ``Num`` constraint on ``a``::                             |
|                                                                                                                    |
|  plus :: NumInst a -> a -> a -> a                                                                                  |
|  plus MkNumInst p q = p + q                                                                                        |
+--------------------------------------------------------------------------------------------------------------------+

