.. raw:: html

  <style> .red {color:red} </style>
  <style> .blk {color:black} </style>
  <style> .center { text-align: center;} </style>
  <style> .strike { text-decoration: line-through;} </style>

.. role:: strike
.. role:: center

.. role:: red
.. role:: blk

Data Types
==========

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

Bottom Type
-----------

In type theory, a theory within mathematical logic, the bottom type is the type
that has no values. It is also called the zero or empty type, and is sometimes
denoted with falsum (⊥).
A function whose return type is bottom cannot return any value.

To signal that a function or computation diverges; in other words, does not
return a result to the caller. (This does not necessarily mean that the program
fails to terminate; a subroutine may terminate without returning to its caller,
or exit via some other means such as a continuation.)

Haskell does not support empty data types. However, in GHC, there is a flag
-XEmptyDataDecls to allow the definition data Empty (with no constructors). The
type Empty is not quite empty, as it contains non-terminating programs and the
undefined constant. The undefined constant is often used when you want
something to have the empty type, because undefined matches any type (so is
kind of a "subtype" of all types), and attempting to evaluate undefined will
cause the program to abort, therefore it never returns an answer.

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

newtypes for composition
~~~~~~~~~~~~~~~~~~~~~~~~

newtypes are often used to wrap a type into a new type for different styles of
composition. For example, the ZipList type supports a different applicative
style composition for a list that zips two lists instead of multiplying them as
in the standard applicative instance of a list.

A newtype may compose differently but it has exactly the same underlying
representation as the original type, in fact it is the same type in a new
bottle that may use different semantics than the original type for the same
composition operators. Types with the same internal representation can be
interconverted safely using the `coerce` operation.

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

Typeclass Constraints
---------------------

Constraint specification:
* Constraints are specified on type variables.

Constraint providers:
* Any function with a constraint in signature will require the constraint at the
call site. That is the caller function or its caller must have the constraint
specified for that variable in its signature. In other words the caller
provides the constraint.
* In a pattern match constraint can also be provided by the data type declaration

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
