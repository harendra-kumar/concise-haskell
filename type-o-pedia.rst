Type-o-pedia
============

* Basics
* Kinds
* Primitive Types
* Basic Haskell Types
* Construction

  * Basic syntax, algebraic, recursive types
  * Ordinary vs Generalized (GADT)
  * Monomorphic vs Polymorphic vs Family
  * Detailed syntax
* Pattern Matching
* Type synonyms
* newtype
* associated types

Basics
------

Terminology
~~~~~~~~~~~

+----------------------------+-----------------------------------------------------------+
| Unboxed                    | Bare type, no wrapping                                    |
+----------------------------+-----------------------------------------------------------+
| Boxed                      | Trackable heap object, wrapped with control info          |
+----------------------------+-----------------------------------------------------------+
| Concrete                   | Has a physical representation (boxed or unboxed)          |
+----------------------------+-----------------------------------------------------------+
| Monomorphic                | Has only one possible representaton                       |
+----------------------------+-----------------------------------------------------------+
| Polymorphic                | Has multiple possible representaton (cannnot be concrete) |
+----------------------------+-----------------------------------------------------------+
| Bottom                     | (_|_) A value that implicitly inhabits all lifted types   |
+----------------------------+-----------------------------------------------------------+
| Unlifted                   | Does not have bottom                                      |
+----------------------------+-----------------------------------------------------------+
| Lifted                     | Has bottom                                                |
+----------------------------+-----------------------------------------------------------+
| Primitives                 | Types which cannot be expressed in Haskell                |
+----------------------------+-----------------------------------------------------------+
| Primops                    | Functions operating on primitive types                    |
+----------------------------+-----------------------------------------------------------+
| Universal Quantification   |                                                           |
+----------------------------+-----------------------------------------------------------+
| Existential Quantification |                                                           |
+----------------------------+-----------------------------------------------------------+

Values, Types & Kinds
~~~~~~~~~~~~~~~~~~~~~

+--------------+--------+
| Compile time | Kinds  |
|              +--------+
|              | Types  |
+--------------+--------+
| Run time     | Values |
+--------------+--------+

+-------------+-------------------------------+----------------------------------------------------------------------------+
| Kinds                                       | ``*`` (Lifted)                                                             |
|                                             +----------------------------------------------------------------------------+
|                                             | TYPE 'IntRep' (Unlifted)                                                   |
+-------------+--------+----------------------+----------------------------------------------------------------------------+
| Types       | Rank1  | Polymorphic Type Fns | t :: k1 -> k2, where k1, k2 are kind variables representing types of rank0 |
|             +--------+----------------------+----------------------------------------------------------------------------+
|             | Rank0  | Type Functions       | t :: * -> * (polymorphic type)                                             |
|             |        +----------------------+----------------------------------------------------------------------------+
|             |        | Concrete Types       | t :: *                                                                     |
+-------------+--------+----------------------+----------------------------------------------------------------------------+
| Values      | Rank2  | Polymorphic Fns      | f :: a -> b where a, b are type variables represeting values up to rank1   |
|             +--------+----------------------+----------------------------------------------------------------------------+
|             | Rank1  | Polymorphic Fns      | f :: a -> b where a, b are type variables representing values of Rank0     |
|             +--------+----------------------+----------------------------------------------------------------------------+
|             | Rank0  | Monomorphic Functions| f :: Char -> Int, monomorphic concrete types                               |
|             |        +----------------------+----------------------------------------------------------------------------+
|             |        | Concrete Values      | f :: Int, monomorphic concrete type                                        |
+-------------+--------+----------------------+----------------------------------------------------------------------------+

Beginner note: Be careful to not confuse identifiers or variables in type space
with those in value space.

Kinds (Type of Type)
--------------------

.. _TYPE 'PtrRepLifted': https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#runtime-representation-polymorphism

Kinds of Concrete Types
~~~~~~~~~~~~~~~~~~~~~~~

+-----------+------+-------------------+-------------+-----------------------+
| Type      |      | Kind              | Runtime Rep | Remarks               |
+===========+======+===================+=============+=======================+
| .. class:: center                                                          |
|                                                                            |
| Unlifted Types                                                             |
| (TYPE is GHC internal representaion)                                       |
+-----------+------+-------------------+-------------+-----------------------+
| Int#      | `::` | TYPE 'IntRep'     | Unboxed     | Direct Reg or Mem     |
+-----------+------+-------------------+-------------+-----------------------+
| Double#   | `::` | TYPE 'DoubleRep'  | Unboxed     | Direct Reg or Mem     |
+-----------+------+-------------------+-------------+-----------------------+
| Array#    | `::` | TYPE 'ArrayRep'   | Unboxed     | Indirect Heap Pointer |
+-----------+------+-------------------+-------------+-----------------------+
| .. class:: center                                                          |
|                                                                            |
| Lifted Types (type * = `TYPE 'PtrRepLifted'`_)                             |
+-----------+------+-------------------+-------------+-----------------------+
| RealWorld | `::` | \*                | NA          | Compile time only     |
+-----------+------+-------------------+-------------+-----------------------+
| Int       | `::` | \*                | Boxed       |                       |
+-----------+------+-------------------+-------------+-----------------------+
| Maybe Int | `::` | \*                | Boxed       |                       |
+-----------+------+-------------------+-------------+-----------------------+

Kinds of Polymorphic Types (Type functions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------+------+-------------------+
| Type      |      | Kind              |
+===========+======+===================+
| Maybe     | `::` | \* -> *           |
+-----------+------+-------------------+
| Either    | `::` | \* -> * -> *      |
+-----------+------+-------------------+

Kind Polymorphism
~~~~~~~~~~~~~~~~~

Type functions can take arguments of kind ``k`` instead of monomorphic kind
``*``. ``k`` is a kind variable which could assume values from ``*``, ``* ->
*``, ``* -> * -> *`` and so on or in other words type functions or polymorphic
types.

``-XTypeInType: -XPolyKinds, -XDataKinds, -XKindSignatures``

Kind check
~~~~~~~~~~

+-----------------+-------------+--------------------------------------+
| Function        | Application | Failure Reason                       |
+-----------------+-------------+--------------------------------------+
| Maybe :: * -> * | Maybe Int#  | Wrong kind ``TYPE 'IntRep'``         |
|                 |             | expected ``*``                       |
+-----------------+-------------+--------------------------------------+

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

+---------------------+----------------------------+
| Importing           | Restrictions               |
+=====================+============================+
| ``import GHC.Prim`` | Cannot use in:             |
|                     | `newtype` definition       |
|                     | , top-level binding        |
|                     | , recursive binding        |
|                     | , non-strict pattern match |
+---------------------+----------------------------+

Basic Haskell Types
-------------------

Construction
------------

Basic Syntax
~~~~~~~~~~~~

.. raw:: html

  <style> .red {color:red} </style>
  <style> .blk {color:black} </style>
  <style> .center {text-align: center;} </style>


.. role:: red
.. role:: blk

Bind type specification to value constructor functions (type & value space bridge):

+------------------+---+--------------------+
| Type Constructor | = | Value Constructors |
+------------------+---+--------------------+

+---------+---------------------+----------------+---+---------------------+-------+-------------------------------------+
|         | Type Constructor    | Type Parameter |   | Value Constructor   |       | Value Constructor                   |
+---------+---------------------+----------------+---+---------------------+-------+-------------------------------------+
| data    | :red:`L`:blk:`ist`  | `a`            | = | :red:`E`:blk:`mpty` | ``|`` | :red:`C`:blk:`ons`  a   (List a)    |
+---------+---------------------+----------------+---+---------------------+-------+-------------------------------------+

Type Constructor
................

* A function that creates a unique type parameterized by the argument types

+------------------+--------+------------+----------------------------------------------+
| Type Entity      |        | Kind       | Description                                  |
+==================+========+============+==============================================+
| List             | ``::`` | ``* -> *`` | Polymorphic type or type constructor         |
+------------------+--------+------------+----------------------------------------------+
| List a           | ``::`` | ``*``      | Concrete type                                |
+------------------+--------+------------+----------------------------------------------+
| `a` must be of kind \*, ``List Maybe`` is invalid while ``List (Maybe Int)`` is valid |
+------------------+--------+------------+----------------------------------------------+

Value Constructors
..................

* Constructors compose values of argument types in a new type
* The return value of all the constructors is treated as representing the same
  type i.e. those values can be swapped in the same slot without a type error.

+-------------------+--------+-------------------------------+
| Value Constructor |        | Type                          |
+===================+========+===============================+
| Empty             | ``::`` | List a                        |
+-------------------+--------+-------------------------------+
| Cons              | ``::`` | Cons :: a -> List a -> List a |
+-------------------+--------+-------------------------------+
| .. class:: center                                          |
|                                                            |
| All arguments must be of kind *                            |
+-------------------+--------+-------------------------------+

Sum & Product
.............

+---------+----------------------------+
| Sum     | data Bool = False | True   |
+---------+----------------------------+
| Product | data Point = Point Int Int |
+---------+----------------------------+

Recursive Types
...............

  data List a = Empty | Cons a (List a)

Monomorphic & Polymorphic
.........................

+------------------+---------------------------------------------------------------------+
| Monomorphic      | data Bool = False | True                                            |
+------------------+---------------------------------------------------------------------+
| Polymorphic      | data List a = Empty | Cons a (List a)                               |
+------------------+---------------------------------------------------------------------+

Ordinary vs Generalized
~~~~~~~~~~~~~~~~~~~~~~~

* Ordinary: return type of all the constructors is the same
* Generalized: return type of constructors could be different

In the following example, ``Term Int``, ``Term Bool``, ``Term a`` & ``Term
(a,b)`` all represent the same type and any of them can be used wherever a
``Term a`` type is required.

::

  data Term a where
      Lit    :: Int -> Term Int
      Succ   :: Term Int -> Term Int
      IsZero :: Term Int -> Term Bool
      If     :: Term Bool -> Term a -> Term a -> Term a
      Pair   :: Term a -> Term b -> Term (a,b)

Pattern matching causes type refinement. e.g. in (Lit i) a is refined to Int ::

  eval :: Term a -> a
  eval (Lit i)      = i
  eval (Succ t)     = 1 + eval t
  eval (IsZero t)   = eval t == 0
  eval (If b e1 e2) = if eval b then eval e1 else eval e2
  eval (Pair e1 e2) = (eval e1, eval e2)

* The general principle is this: type refinement is only carried out based on
  user-supplied type annotations. So if no type signature is supplied for eval,
  no type refinement happens
* You cannot use a deriving clause for a GADT
* When pattern-matching against data constructors drawn from a GADT, for example in a case expression, the following rules apply:

  * The type of the scrutinee must be rigid.
  * The type of the entire case expression must be rigid.
  * The type of any free variable mentioned in any of the case alternatives must be rigid.
  * A type is “rigid” if it is completely known to the compiler at its binding
    site. The easiest way to ensure that a variable a rigid type is to give it a
    type signature.

Polymorphic & Polymorphic Families
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------+---------------------------------------------------------------------+---------------------------------------------------+
| Polymorphic      | data List a = Empty | Cons a (List a)                               | Every instance uses the same constructor template |
|                  |                                                                     |                                                   |
|                  | List Char                                                           |                                                   |
|                  |                                                                     |                                                   |
|                  | List ()                                                             |                                                   |
+------------------+---------------------------------------------------------------------+---------------------------------------------------+
| Polymorphic      | data family List a                                                  | Every instance defines its own constructors       |
|                  |                                                                     |                                                   |
|                  | data instance List Char = Empty | Cons Char (List Char) | List Char |                                                   |
|                  |                                                                     |                                                   |
| (Family)         | data instance List ()   = Count Int                                 |                                                   |
+------------------+---------------------------------------------------------------------+---------------------------------------------------+

Data instance definitions can be likened to function definitions with pattern
matching except these are type functions.

Detailed Syntax
~~~~~~~~~~~~~~~

+------------------------------------------------------------+-------------------------------------------------------+
| Haskell98 Syntax                                           | GADT Syntax                                           |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| GADT Universal Quantification                                                                                      |
+------------------------------------------------------------+-------------------------------------------------------+
|                                                            | | data T a where      -- 'a' has no scope             |
|                                                            | |  T1,T2 :: b -> T b  -- forall b. b -> T b           |
|                                                            | |  T3 :: T a          -- forall a. T a                |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| GADT Type parameters have no scope (only the kind is required)                                                     |
+------------------------------------------------------------+-------------------------------------------------------+
|                                                            | | data Bar a b where ...                              |
|                                                            | | data Bar :: * -> * -> * where ...                   |
|                                                            | | data Bar a :: (* -> \*) where ...                   |
|                                                            | | data Bar a (b :: * -> \*) where ...                 |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Derivation                                                                                               |
+------------------------------------------------------------+-------------------------------------------------------+
| data Maybe a = Nothing | Just a                            | | data Maybe a where {                                |
|     deriving( Eq, Ord )                                    | |     Nothing :: Maybe a                              |
|                                                            | |     Just    :: a -> Maybe a                         |
|                                                            | |   } deriving( Eq, Ord )                             |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Typeclass Constraint                                                                                               |
+------------------------------------------------------------+-------------------------------------------------------+
| data Set a = Eq a => MkSet [a]                             |  | data Set a where                                   |
|                                                            |  |   MkSet :: Eq a => [a] -> Set a                    |
+------------------------------------------------------------+-------------------------------------------------------+
| * Construction `requires` ``Eq a``: makeSet :: :red:`Eq a =>` [a] -> Set a; makeSet xs = MkSet (nub xs)            |
| * Pattern match `provides` ``Eq a``: insert a (MkSet as) | a :red:`\`elem\`` as = MkSet as                         |
| * Note: Haskell98 `requires` instead of `providing` ``Eq a`` in pattern match.                                     |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Records                                                                                                            |
+------------------------------------------------------------+-------------------------------------------------------+
|                                                            | | data P where                                        |
|                                                            | |   Adlt :: {nm :: String, children :: [P]} -> P      |
|                                                            | |   Chld :: Show a => {nm :: !String, fny :: a} -> P  |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XExistentialQuantification                                                                                        |
+------------------------------------------------------------+-------------------------------------------------------+
| Quantified type variables that do not appear in the result type of a constructor are existentials.                 |
| They are confined within the data type and can only be used within this binding group.                             |
+------------------------------------------------------------+-------------------------------------------------------+
| data Foo = forall a. Foo a (a -> a)                        | data Foo where { Foo :: a -> (a -> a) -> Foo }        |
+------------------------------------------------------------+-------------------------------------------------------+
| Packs opaque data and function together. Equivalent to: Foo :: (exists a . (a, a -> a)) -> Foo                     |
+------------------------------------------------------------+-------------------------------------------------------+
| data Foo = forall a. Show a => Foo a (a -> a)              | data Foo where {Foo :: Show a => a -> (a -> a) -> Foo}|
+------------------------------------------------------------+-------------------------------------------------------+
| Constraint is available on pattern match: f (Foo v fn) = show (fn v)                                               |
+------------------------------------------------------------+-------------------------------------------------------+
| | data Counter a = forall self. NewCounter                 | | data Counter a where                                |
| |   { _this    :: self                                     | |     NewCounter :: { _this    :: self                |
| |   , _inc     :: self -> self                             | |                   , _inc     :: self -> self        |
| |   , _display :: self -> IO ()                            | |                   , _display :: self -> IO ()       |
| |   , tag      :: a                                        | |                   , tag      :: a                   |
| |   }                                                      | |                   } -> Counter a                    |
+------------------------------------------------------------+-------------------------------------------------------+
| Record constructors:                                                                                               |
|                                                                                                                    |
| * Only ``tag`` will get a selector function and can be updated                                                     |
| * All other fields can be used in construction and pattern matching only.                                          |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| Strictness Annotations                                                                                             |
+------------------------------------------------------------+-------------------------------------------------------+
|                                                            | | data Term a where                                   |
|                                                            | |   Lit :: !Int -> Term Int                           |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XEmptyDataDecls                                                                                                   |
+------------------------------------------------------------+-------------------------------------------------------+
| data T a    -- T :: * -> *                                 |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| Infix type constructor                                                                                             |
+------------------------------------------------------------+-------------------------------------------------------+
| data a :\*: b = Foo a b                                    |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XTypeOperators                                                                                                    |
+------------------------------------------------------------+-------------------------------------------------------+
| data a + b = Plus a b                                      |                                                       |
+------------------------------------------------------------+-------------------------------------------------------+


Dictionary Reification
~~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------+-------------------------------------------------------+
| data NumInst a = Num a => MkNumInst                        | | data NumInst a where                                |
|                                                            | |  MkNumInst :: Num a => NumInst a                    |
+------------------------------------------------------------+-------------------------------------------------------+
| ``MkNumInst`` reifies ``Num`` dictionary: plus :: NumInst a -> a -> a -> a; plus MkNumInst p q = p + q             |
+------------------------------------------------------------+-------------------------------------------------------+

Heap Representation
~~~~~~~~~~~~~~~~~~~
TODO: Memory representation of the type (i.e. a closure)
with pointers to the contained types.

Deconstruction (Pattern Matching)
---------------------------------

* Pattern matching is the only way to look inside a constructed data
* Just swap the LHS and RHS of constructor application

* let
* case
* function
* where

Existential Quantification:

* Remember each instance is independent isolated type space, type cannot escape
  via pattern match
* In general, you can only pattern-match on an existentially-quantified
  constructor in a case expression or in the patterns of a function definition.
  You can’t pattern-match on an existentially quantified constructor in a let
  or where group of bindings.

Type Synonyms
-------------

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

* ''newtype'' takes exactly one value constructor with exactly one field no less no more. It can take multiple type parameters though. Its basic purpose is to wrap multiple existing types into a new type. More about newtype vs data at :

  * http://stackoverflow.com/questions/21327740/strict-single-constructor-single-field-data-declaration-vs-newtype/21331284#21331284.
  * http://stackoverflow.com/questions/2649305/why-is-there-data-and-newtype-in-haskell
* With ''data'' keyword you cannot infer the complete type by looking at just one value constructor e.g.:

::

  Prelude Control.Exception> data MyData a b = A a | B b deriving Show
  Prelude Control.Exception> :t A
  A :: a -> MyData a b
  Prelude Control.Exception> :t A "X"
  A "X" :: MyData [Char] b
  Prelude Control.Exception> :t B "Y"
  B "Y" :: MyData a [Char]

However since ''newtype'' allows only single constructor and field the type can be inferred easily by looking at a single value:

::

  Prelude Control.Exception> newtype MyData a b = A (a, b) deriving Show
  Prelude Control.Exception> :t A (4, "A")
  A (4, "A") :: Num a => MyData a [Char]
  Prelude Control.Exception>

* You can’t use existential quantification for newtype declarations.

Associated Types
----------------

::

    class GMapKey k where
      data GMap k :: * -> *

      empty       :: GMap k v
      lookup      :: k -> GMap k v -> Maybe v
      insert      :: k -> v -> GMap k v -> GMap k v

    instance GMapKey Int where
      data GMap Int v        = GMapInt (Data.IntMap.IntMap v)

      empty                  = GMapInt Data.IntMap.empty
      lookup k   (GMapInt m) = Data.IntMap.lookup k m
      insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)

    instance GMapKey () where
      data GMap () v           = GMapUnit (Maybe v)

      empty                    = GMapUnit Nothing
      lookup () (GMapUnit v)   = v
      insert () v (GMapUnit _) = GMapUnit $ Just v
