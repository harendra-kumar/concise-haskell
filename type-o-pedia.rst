Type-o-pedia
------------

Three spaces
============

+---------------------------------------+--------------------------+
| Compile time                          | Run time                 |
+------------+--------------------------+--------------------------+
| Kind Space | Type Space               | Value Space              |
+------------+----------------+---------+-----------------+--------+
| Kinds      | Type Functions | Types   | Value Functions | Values |
+------------+----------------+---------+-----------------+--------+

Beginner note: Be careful to not confuse identifiers or variables in type space
with those in value space.

Terminology
===========

+-------------+-----------------------------------------------------------+
| Unboxed     | Bare type, no wrapping                                    |
+-------------+-----------------------------------------------------------+
| Boxed       | Trackable heap object, wrapped with control info          |
+-------------+-----------------------------------------------------------+
| Concrete    | Has a physical representation (boxed or unboxed)          |
+-------------+-----------------------------------------------------------+
| Monomorphic | Has only one possible representaton                       |
+-------------+-----------------------------------------------------------+
| Polymorphic | Has multiple possible representaton (cannnot be concrete) |
+-------------+-----------------------------------------------------------+
| Bottom      | (_|_) A value that implicitly inhabits all lifted types   |
+-------------+-----------------------------------------------------------+
| Unlifted    | Does not have bottom                                      |
+-------------+-----------------------------------------------------------+
| Lifted      | Has bottom                                                |
+-------------+-----------------------------------------------------------+
| Primitives  | Types which cannot be expressed in Haskell                |
+-------------+-----------------------------------------------------------+
| Primops     | Functions operating on primitive types                    |
+-------------+-----------------------------------------------------------+

Kinds (Type of Type)
====================

.. _TYPE 'PtrRepLifted': https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#runtime-representation-polymorphism

+-------+---------------------------------------+--------------------------+
| ``*`` | Represents Monomorphic & lifted types | `TYPE 'PtrRepLifted'`_   |
+-------+---------------------------------------+--------------------------+

+----------+------------------------------+--------------+-------------+
| Category | Example Types & Kinds        | Physical Rep | Shape       |
+==========+==============================+==============+=============+
| Unlifted | Int# :: TYPE 'IntRep'        | Direct       | Monomorphic |
|          +------------------------------+--------------+-------------+
|          | Double# :: TYPE 'DoubleRep'  | Direct       | Monomorphic |
|          +------------------------------+--------------+-------------+
|          | Array# :: TYPE 'ArrayRep'    | Heap pointer | Monomorphic |
+----------+------------------------------+--------------+-------------+
| Lifted   | RealWorld :: *               | Not concrete | Monomorphic |
|          +------------------------------+--------------+-------------+
|          | Int :: *                     | Boxed        | Monomorphic |
|          +------------------------------+--------------+-------------+
|          | Maybe :: * -> *              | Not concrete | Polymorphic |
+----------+------------------------------+--------------+-------------+

Kind check:

+-----------------+-------------+--------------------------------------+
| Function        | Application | Failure Reason                       |
+-----------------+-------------+--------------------------------------+
| Maybe :: * -> * | Maybe Int#  | Wrong kind ``TYPE 'IntRep'``         |
|                 |             | expected ``*``                       |
+-----------------+-------------+--------------------------------------+

Primitive Types
===============

* `ghc-prim <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.0.0/GHC-Prim.html>`_: Describes primitive data types and primops
* ``import GHC.Prim``

Restrictions; cannot use in:

* `newtype` definition
* top-level binding
* recursive binding
* non-strict pattern match

Unlifted Primitives
~~~~~~~~~~~~~~~~~~~

Unlifted primitive types, values, and operations have a # suffix:

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
~~~~~~~~~~~~~~~~~~~

+------------+
| RealWorld  |
+------------+

Kind Polymorphism
=================

Type functions are first class objects in type space like value functions are
first class objects in value space. Which means type functions can be passed
around like normal types.

Type functions can take arguments of kind ``k`` instead of monomorphic kind
``*``. ``k`` is a kind variable which could assume values from ``*``, ``* ->
*``, ``* -> * -> *`` and so on or in other words type functions or polymorphic
types.

For example if we pass ``Maybe`` to a type constructor or type function then we
are into kind polymorphism.

``-XTypeInType: -XPolyKinds, -XDataKinds, -XKindSignatures``

Basic Haskell Types
===================

Data Declarations
=================

Binds types to values (type & value space bridge):

+------------------+---+-------------------+
| Type Constructor | = | Value Constructor |
+------------------+---+-------------------+

* Notation: type constructor, value constructor have first
  letter capitalized.
* `a` is a type variable of kind *

+---------+------------------+----------------+---+-----------------+-------+-------------------------------------+
|         | Type Constructor | Type Parameter |   | Constructor     |       | Constructor (Arg1 type) (Arg2 Type) |
+---------+------------------+----------------+---+-----------------+-------+-------------------------------------+
| data    | List             | a              | = | Empty           | ``|`` | Cons        a           (List a)    |
+---------+------------------+----------------+---+-----------------+-------+-------------------------------------+

Type Constructors: Functions in type space
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------+--------+------------+--------------------------------------+
| Type Entity      |        | Kind       | Description                          |
+==================+========+============+======================================+
| List a           | ``::`` | ``* -> *`` | Polymorphic type or type constructor |
+------------------+--------+------------+--------------------------------------+
| List Int         | ``::`` | ``*``      | Concrete type                        |
+------------------+--------+------------+--------------------------------------+

Value Constructors: Functions that create values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------+--------+-------------------------------+
| Value Constructor |        | Type                          |
+===================+========+===============================+
| Empty             | ``::`` | List a                        |
+-------------------+--------+-------------------------------+
| Cons              | ``::`` | Cons :: a -> List a -> List a |
+-------------------+--------+-------------------------------+

Deconstruction aka Pattern Matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Pattern matching is the only way to look inside a constructed data
* Just swap the LHS and RHS of constructor application

TODO: Memory representation of the type (i.e. a closure)
with pointers to the contained types.

More on Data Declarations
~~~~~~~~~~~~~~~~~~~~~~~~~

+------------------------------------------------------------+--------------------------------------------------+
| data T a    -- T :: * -> *                                 | -XEmptyDataDecls                                 |
+------------------------------------------------------------+--------------------------------------------------+
| ``data a :*: b = Foo a b``                                 | Infix type constructor                           |
+------------------------------------------------------------+--------------------------------------------------+
| data a + b = Plus a b                                      | -XTypeOperators                                  |
+------------------------------------------------------------+--------------------------------------------------+
| data Foo = forall a. MkFoo a (a -> Bool) | Nil             | -XExistentialQuantification (pack data + fn)     |
+------------------------------------------------------------+--------------------------------------------------+
| data Baz = forall a. Eq a => Baz1 a a                      | -XExistentialQuantification (specify constraints)|
+------------------------------------------------------------+--------------------------------------------------+

Type Synonyms
~~~~~~~~~~~~~

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

::

  class a :=: b where ...

Algebraic Types
===============

+---------+----------------------------+
| Sum     | data Bool = False | True   |
+---------+----------------------------+
| Product | data Point = Point Int Int |
+---------+----------------------------+

Recursive Types
===============

  data List a = Empty | Cons a (List a)

Polymorphic Types
=================

+------------------+---------------------------------------------------------------------+
| Monomorphic      | data Bool = False | True                                            |
+------------------+---------------------------------------------------------------------+
| Polymorphic      | data List a = Empty | Cons a (List a)                               |
|                  |                                                                     |
| (Fixed Shape)    | List Char                                                           |
|                  |                                                                     |
|                  | List ()                                                             |
+------------------+---------------------------------------------------------------------+
| Polymorphic      | data family List a                                                  |
|                  |                                                                     |
| (Variable Shape) | data instance List Char = Empty | Cons Char (List Char) | List Char |
|                  |                                                                     |
| (Family)         | data instance List ()   = Count Int                                 |
+------------------+---------------------------------------------------------------------+

Data instance definitions can be likened to function definitions with pattern
matching except these are type functions.

Associated Types
================

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

newtype
=======

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
