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

.. contents:: Table of Contents
   :depth: 1

Terminology
-----------

+----------------------------+-----------------------------------------------------------------+
| Rigid type                 | Type is fixed by annotation (signature) and not determined by   |
|                            | inference.                                                      |
+----------------------------+-----------------------------------------------------------------+
| Rigid type variable        | The scope (quantification) of the type variable is fixed by     |
| (skolem)                   | annotation.                                                     |
+----------------------------+-----------------------------------------------------------------+
| Unification                | algorithmic process of solving equations between symbolic       |
|                            | expressions.                                                    |
+----------------------------+-----------------------------------------------------------------+
| Unification of type vars   | A type variable can be `unified` with other type variable of    |
|                            | the same name if both are in the same quantification scope and  |
|                            | therefore are referring to one and the same thing.              |
+----------------------------+-----------------------------------------------------------------+
| Coerce                     | Cast a type into another (compatible) type                      |
+----------------------------+-----------------------------------------------------------------+
| Unboxed                    | Bare physical representation, no wrapping or indirection layer  |
+----------------------------+-----------------------------------------------------------------+
| Boxed                      | Physical representation wrapped with control info               |
|                            | (trackable heap object).                                        |
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

Lifted Types
------------

Semantics: Adding Bottom
~~~~~~~~~~~~~~~~~~~~~~~~

Semantically adding a bottom value to the set of values denoted by a
type is called lifting the type.

A bottom value is implicit in lifted types. For example, you can imagine:

``data () = ()`` <=> ``data () = () | ⊥``

A type is lifted if and only if it has bottom as an element. The concept of
bottom element is necessary to represent some important semantic properties of
Haskell programs like lazy evaluation, non-termination, partial functions,
errors and exceptions.

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
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Expressions evaluating to bottom                                            |
+--------------------------------------------+--------------------------------+
| non-termination                            | let x = x                      |
+--------------------------------------------+--------------------------------+
| partial functions                          | head []                        |
+--------------------------------------------+--------------------------------+

Runtime Representation
~~~~~~~~~~~~~~~~~~~~~~

Operationally, data constructors of a lifted type and terms with lifted types
may be represented by closures and can be lazily evaluated.  In the context of
lazy evaluation (graph reduction) the unevaluated expression can be thought of
as bottom which can be evaluated on demand to determine the actual value.

Terms with unlifted types must not be represented by closures, which implies
that any unboxed value is necessarily unlifted. We distinguish between lifted
and unlifted types by ascribing them different kinds.

.. _Primitive Types: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.0.0/GHC-Prim.html

`Primitive Types`_
------------------

Unlifted Primitives
~~~~~~~~~~~~~~~~~~~

Unlifted types can be boxed (e.g. Array types) or unboxed.

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

Generalized Algebraic Data Type (GADT) Syntax
---------------------------------------------

+------------------------------------------------------------------+
| .. class :: center                                               |
|                                                                  |
|  -XGADTSyntax                                                    |
+------------------------------------------------------------------+
| Standard algebraic data type syntax                              |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a = Empty | Cons a (List a)                           |
+------------------------------------------------------------------+
| * Each data constructor has the same return type which is        |
|   implicit and the same as the data type.                        |
| * The data type parameter scopes over the constructors and is    |
|   used as a type parameter in the constructors.                  |
+------------------------------------------------------------------+
| Generalized (GADT) Syntax                                        |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a where     -- 'a' has no scope, only a placeholder   |
|    Empty :: List b                                               |
|    Cons  :: c -> List c -> List c                                |
+------------------------------------------------------------------+
| * GADT syntax essentially specifies two things, the arity of the |
|   type constructor and signatures of all data constructors       |
|   explicitly.                                                    |
| * It allows the return type of each data constructor to be       |
|   different.                                                     |
| * The data type parameter 'a' is only a placeholder and has no   |
|   scope. It indicates only the arity of the type function.       |
| * Type variables across different constructors are not related.  |
| * Type variables featuring in the return type of a constructor   |
|   are implicitly universally quantified.                         |
| * Type variables not featuring in the return type of a           |
|   constructor are implicitly existentially quantified            |
| * The actual value of variables 'b' and 'c' is determined by     |
|   inference based on the usage of the constructors.              |
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

Misc Data Construction Syntax
-----------------------------

+------------------------------------------------------------+-------------------------------------------------------+
| Regular ADT Syntax                                         | GADT Syntax                                           |
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
-------

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
| Until the brain gets trained, it is pretty confusing that the types of the  |
| selector functions are different from what they seem to be from the code:   |
+-----------------------------------+-----------------------------------------+
| ::                                | ::                                      |
|                                   |                                         |
|  data R =                         |  --                                     |
|    R {                            |                                         |
|        x :: String                |  x  :: R -> String                      |
|      , y :: Int                   |  y  :: R -> Int                         |
|    }                              |                                         |
+-----------------------------------+-----------------------------------------+
| `-XDuplicateRecordFields` (8.0.1) allows using identical fields in different|
| records even in the same module. Selector functions and updates are         |
| disambiguated using the type of the field or the record.                    |
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
| Note that selector functions are symbols but field names are literals i.e.  |
| you cannot say x = y and then use x in place of y as a field name. x will   |
| refer to the selector function, when used as a field name it will refer to  |
| field named "x" rather than "y".                                            |
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
| ``..`` expands to missing        | ``f (R {x = "a", ..}) = R{x = "b", ..}`` |
| `in-scope` record fields         |                                          |
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

Polymorphic Algebraic Data Types
--------------------------------

Data Type Declaration
~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------+-----+-------------------------------------------------------------------+
| .. class:: center                              |     | .. class:: center                                                 |
|                                                |     |                                                                   |
| Type Level Function                            |     | Data Constructor Templates                                        |
+=========+=====================+================+=====+=====================+=======+=====================================+
|         | Type Constructor    |      Parameter |     | Data Constructor    |       | Data Constructor                    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| data    | :red:`L`:blk:`ist`  | `a`            |  =  | :red:`E`:blk:`mpty` | ``|`` | :red:`C`:blk:`ons`  a   (List a)    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+

Type Constructor
^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------------------+
| A type level function to create a type from existing types                              |
+----------------------+--------+------------------+--------------------------------------+
| Type                 |        | Kind             | Description                          |
+======================+========+==================+======================================+
| List                 | ``::`` | ``Type -> Type`` | Polymorphic type or type constructor |
+----------------------+--------+------------------+--------------------------------------+
| The signature implies that the parameter `a` must be a concrete type of kind ``Type``   |
+-----------------------------------------------------------------------------------------+
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
^^^^^^^^^^^^^^^^^

+--------------------------------------------------------------------------------------------------------+
| A data level function to create a value of the corresponding type                                      |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Data Constructor  |        | Type                          | Description                               |
+===================+========+===============================+===========================================+
| Empty             | ``::`` | List a                        | Create a new value (denoting empty list)  |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Cons              | ``::`` | Cons :: a -> List a -> List a | Compose two values (`a` and `List a`)     |
+-------------------+--------+-------------------------------+-------------------------------------------+
| The signatures imply that the arguments of constructors must be concrete types of kind ``Type``        |
+--------------------------------------------------------------------------------------------------------+

Typeclass Constraints
~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Constraint (:red:`Deprecated Haskell 98 style`, -XDatatypeContexts)                                      |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         |                                                       |
|                                                            |                                                       |
|   data Eq a => Set a = MkSet [a]                           |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+
| * Construction `requires` ``Eq a``: makeSet :: :red:`Eq a =>` [a] -> Set a; makeSet xs = MkSet (nub xs)            |
| * Pattern match also `requires`                                                                                    |
|   ``Eq a``: insert :: :red:`Eq a =>` a -> Set a; insert a (MkSet as) | a :red:`\`elem\`` as = MkSet as             |
| * It is recommened to use the GHC style typeclass constraint which provides the constraint on pattern match        |
|   instead of requiring it.                                                                                         |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Constraint (Available only with -XGADTs or -XExistentialQuantification)                                  |
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

Quantification
~~~~~~~~~~~~~~

+--------------------------------------------------------------------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XExistentialQuantification                                                                                        |
+--------------------------------------------------------------------------------------------------------------------+
| Quantified type variables that appear in arguments but not in the result type for any constructor are              |
| `existentials`. The existence, visibility or scope of these type variables is localized to the given constructor.  |
| They will typecheck with other instances only within this local scope. In other words, they cannot be unified with |
| variables outside this scope.                                                                                      |
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

Pattern Matching
----------------

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
~~~~~~~~~~~~~~~~~~~~~~~

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
----------------

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
----------------------

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

Pattern Match Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Type Synonyms
-------------

+-----------------------------------------------------------------------------+
| A type synonym is a function that generates a synonym of an existing type   |
| or its specialization.                                                      |
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
| `W is not a data constructor`, it does not construct algebraic data, it is  |
| just a type level (compile time) wrapper to wrap the original type into a   |
| new type N.                                                                 |
| Since W is a type wrapper and not a data constructor:                       |
|                                                                             |
| * you cannot provide multiple arguments to W.                               |
| * you can’t use existential quantification for newtype declarations.        |
| * it does not lift the wrapped type, however it wraps only lifted types.    |
| * unlike a data constructor it has no runtime overhead. The wrapper is used |
|   for type checking at compile time and discarded thereafter.               |
+-----------------------------------------------------------------------------+
| However just like data constructors, you can:                               |
|                                                                             |
| * pattern match on wrapper W to extract the original type. The pattern      |
|   match is purely a compile time operation equivalent to coercing the type  |
|   into the original type.                                                   |
| * use a `deriving` clause                                                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  newtype WrapInt = WrapInt Int                                              |
|  newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  |
+-----------------------------------------------------------------------------+
| Unlike a type synonym the type created by `newtype` is a distinct type      |
| and cannot be used in place of the original type.                           |
+-----------------------------------------------------------------------------+
| Newtypes may also be used to define recursive types. For example:           |
| ::                                                                          |
|                                                                             |
|  newtype List a = In (Maybe (a, List a))                                    |
+-----------------------------------------------------------------------------+

+----------------------------+------------------------+-------------------------------+
| data                       | type                   | newtype                       |
+============================+========================+===============================+
| ``data Count = Count Int`` | ``type Count = Int``   | ``newtype Count = Count Int`` |
+----------------------------+------------------------+-------------------------------+
| ``Count`` and ``Int``      | ``Count`` and ``Int``  | ``Count`` and ``Int`` are     |
| are distinct types         | refer to exactly the   | distinct types                |
+----------------------------+ same type and can be   +-------------------------------+
| ``Count`` is a constructor | used interchangeably   | ``Count`` is a type level     |
| wrapping an ``Int``        |                        | wrapper wrapping an ``Int``   |
+----------------------------+                        +-------------------------------+
| Physically ``Count`` is a  |                        | ``Count`` does not exist      |
| closure on heap            |                        | physically it is removed after|
| wrapping the ``Int``       |                        | type checking                 |
| closure                    |                        |                               |
+----------------------------+------------------------+-------------------------------+

Type Signatures
---------------

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

Constraints in types
--------------------

Constraints are just handled as types of the kind `Constraint`.

+-----------------------------------------------------------------------------+
| Typeclass Constraint (saturated applications to type classes)               |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Implicit parameter constraints (``-XImplicitParams``)                       |
+-----------------------------------------------------------------------------+
| ``?x :: Int``                                                               |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Equality Constraint (``-XTypeFamilies`` or ``-XGADTs``)                     |
+-----------------------------------------------------------------------------+
| In the presence of type families, whether two types are equal cannot        |
| generally be decided locally. Hence, the contexts of function signatures    |
| may include equality constraints of the form ``t1 ~ t2``, as in the         |
| following example:                                                          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2)               |
|              => c1 -> c2 -> c2                                              |
+-----------------------------------------------------------------------------+
| In general, the types t1 and t2 of an equality constraint may be arbitrary  |
| monotypes; i.e., they may not contain any quantifiers, independent of       |
| whether higher-rank types are otherwise enabled.                            |
+-----------------------------------------------------------------------------+
| Equality constraints in class and instance contexts enable a simple         |
| translation of programs using functional dependencies into programs using   |
| family synonyms instead essentially giving a name to the functional         |
| dependency.                                                                 |
+-----------------------------------+-----------------------------------------+
| ::                                |                                         |
|                                   |                                         |
|  class C a b | a -> b             | class (F a ~ b) => C a b where type F a |
+-----------------------------------+-----------------------------------------+
| ``~~`` denotes kind-heterogeneous equality, which relates two types of      |
| potentially different kinds. The kinds of ``~`` and ``~~`` are:             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  (~)  :: forall k. k -> k -> Constraint                                     |
|  (~~) :: forall k1 k2. k1 -> k2 -> Constraint                               |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Coercible Constraint                                                        |
+-----------------------------------------------------------------------------+
| The constraint ``Coercible t1 t2`` is similar to ``t1 ~ t2``, but denotes   |
| representational equality between ``t1`` and ``t2`` in the sense of Roles   |
| (Roles). It is exported by ``Data.Coerce``.                                 |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| ``-XConstraintKinds``                                                       |
+-----------------------------------------------------------------------------+
| ``GHC.Exts`` exports the kind ``Constraint``                                |
+-----------------------------------------------------------------------------+
| Any type of the kind ``Constraint`` can be used as a constraint.            |
| The following things have kind ``Constraint``:                              |
+-----------------------------------------------------------------------------+
| Individual constraints described earlier.                                   |
+-----------------------------------------------------------------------------+
| Tuples, all of whose component types have kind ``Constraint`` e.g.          |
| ``(Show a, Ord a)``                                                         |
+-----------------------------------------------------------------------------+
| Constraint synonyms                                                         |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type Stringy a = (Read a, Show a)                                          |
|  foo :: Stringy a => a -> (String, String -> a)                             |
|  foo x = (show x, read)                                                     |
+-----------------------------------------------------------------------------+
| Anything that the user has declared to have kind ``Constraint`` e.g.        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type Foo (f :: \* -> Constraint) = forall b. f b => b -> b                 |
|                                                                             |
|  type family Typ a b :: Constraint                                          |
|  type instance Typ Int  b = Show b                                          |
|  type instance Typ Bool b = Num b                                           |
|                                                                             |
|  func :: Typ a b => a -> b -> b                                             |
|  func = ...                                                                 |
+-----------------------------------------------------------------------------+
| Permitting more general constraints can cause type checking to loop, you    |
| must use ``-XUndecidableInstances`` to signal that you don’t mind if the    |
| type checker fails to terminate.                                            |
+-----------------------------------------------------------------------------+

Data.Type.Equality (base package)
---------------------------------

Definition of propositional equality (:~:). Pattern-matching on a variable of
type (a :~: b) produces a proof that a ~ b.

References
----------

* https://www.microsoft.com/en-us/research/wp-content/uploads/2012/01/icfp12.pdf Equality proofs and deferred type errors A compiler pearl
