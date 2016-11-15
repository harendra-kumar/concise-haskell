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

Basics
------

Terminology
~~~~~~~~~~~

+----------------------------+-----------------------------------------------------------------+
| Type                       | Distinguishes a class of values (e.g. Integer or String)        |
+----------------------------+-----------------------------------------------------------------+
| Kind                       | Distinguishes a class of types (e.g. Lifted or unlifted)        |
+----------------------------+-----------------------------------------------------------------+
| Rigid type                 | Type is fixed by annotation (signature) and not determined by   |
|                            | inference.                                                      |
+----------------------------+-----------------------------------------------------------------+
| Concrete                   | Has a physical representation (boxed or unboxed)                |
+----------------------------+-----------------------------------------------------------------+
| Monomorphic                | Has only one possible representation                            |
+----------------------------+-----------------------------------------------------------------+
| Polymorphic                | Has multiple possible representations (cannnot be concrete)     |
+----------------------------+-----------------------------------------------------------------+
| Unboxed                    | Bare type, no wrapping                                          |
+----------------------------+-----------------------------------------------------------------+
| Boxed                      | Trackable heap object, wrapped with control info                |
+----------------------------+-----------------------------------------------------------------+
| Bottom (_|_)               | Further evaluation of lazy value not possible, found the bottom |
+----------------------------+-----------------------------------------------------------------+
| Unlifted                   | Not lazily evaluated; does not have a concept of bottom         |
+----------------------------+-----------------------------------------------------------------+
| Lifted                     | Supports lazy evaluation; contains an implicit bottom value     |
+----------------------------+-----------------------------------------------------------------+
| Primitives                 | Types which cannot be expressed in Haskell                      |
+----------------------------+-----------------------------------------------------------------+
| Primops                    | Functions operating on primitive types                          |
+----------------------------+-----------------------------------------------------------------+
| Universal Quantification   | forall `a`: the proposition is true for all values of a         |
+----------------------------+-----------------------------------------------------------------+
| Existential Quantification | exists `a`: the proposition is true for some values of a        |
+----------------------------+-----------------------------------------------------------------+
| Open                       | Open to extension, can be extended (e.g. open type families)    |
+----------------------------+-----------------------------------------------------------------+
| Closed                     | Closed to extension, cannot be extended                         |
|                            | (e.g. closed type families)                                     |
+----------------------------+-----------------------------------------------------------------+

Values, Types & Kinds
~~~~~~~~~~~~~~~~~~~~~

+--------------+--------+
| Compile time | Kinds  |
|              +--------+
|              | Types  |
+--------------+--------+
| Run time     | Values |
+--------------+--------+

Kinds
~~~~~

+----------------------+----------------------+--------------------------------------------------------------------------------+
| Kinds                | Lifted Types         | ``*``                                                                          |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Unlifted Types       | ``TYPE 'IntRep'``, ``TYPE 'DoubleRep'`` ...                                    |
+----------------------+----------------------+--------------------------------------------------------------------------------+

Types & Type Functions
~~~~~~~~~~~~~~~~~~~~~~

+-------------+--------+----------------------+--------------------------------------------------------------------------------+
| Types       | Rank1  | Polymorphic Type Fns | ``t :: k1 -> k2``, where k1, k2 are kind variables representing types of rank0 |
|             +--------+----------------------+--------------------------------------------------------------------------------+
|             | Rank0  | Type Functions       | ``t :: * -> *`` (polymorphic type)                                             |
|             |        +----------------------+--------------------------------------------------------------------------------+
|             |        | Concrete Types       | ``t :: *``                                                                     |
+-------------+--------+----------------------+--------------------------------------------------------------------------------+

Values & Value Functions
~~~~~~~~~~~~~~~~~~~~~~~~

+-------------+--------+----------------------+--------------------------------------------------------------------------------+
| Values      | Rank2  | Polymorphic Fns      | ``f :: a -> b`` where a, b are type variables representing values up to rank1  |
|             +--------+----------------------+--------------------------------------------------------------------------------+
|             | Rank1  | Polymorphic Fns      | ``f :: a -> b`` where a, b are type variables representing values of Rank0     |
|             +--------+----------------------+--------------------------------------------------------------------------------+
|             | Rank0  | Monomorphic Functions| ``f :: Char -> Int``, monomorphic concrete types                               |
|             |        +----------------------+--------------------------------------------------------------------------------+
|             |        | Concrete Values      | ``f :: Int``, monomorphic concrete type                                        |
+-------------+--------+----------------------+--------------------------------------------------------------------------------+

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
| (TYPE is GHC internal representation)                                      |
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

+---------------------+--------------------------------+
| Importing           | Restrictions                   |
+=====================+================================+
| ``import GHC.Prim`` | Cannot use unlifted types in:  |
|                     | `newtype` definition           |
|                     | , top-level binding            |
|                     | , recursive binding            |
|                     | , lazy pattern match           |
+---------------------+--------------------------------+

Basic Haskell Types
-------------------

+---------+
| Int     |
+---------+
| Integer |
+---------+
| Float   |
+---------+
| Double  |
+---------+
| Bool    |
+---------+
| Char    |
+---------+

Construction
------------

Basic Syntax
~~~~~~~~~~~~

+--------------------------------------------------------------------------------------------------------------------------+
| Bind a type instance to value constructor functions                                                                      |
+------------------------------------------------+-----+-------------------------------------------------------------------+
| .. class:: center                              |     | .. class:: center                                                 |
|                                                |     |                                                                   |
| Type Space                                     |     | Value Space                                                       |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
|         | Type Constructor    |      Parameter |     | Value Constructor   |       | Value Constructor                   |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| data    | :red:`L`:blk:`ist`  | `a`            |  =  | :red:`E`:blk:`mpty` | ``|`` | :red:`C`:blk:`ons`  a   (List a)    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| where parameter `a` as well as all argument types of value constructors must be a type of kind \*                        |
+--------------------------------------------------------------------------------------------------------------------------+

Type Constructor
................

+-------------------------------------------------------------------------------------------+
| A (possibly parameterized) type function to instantiate a new type                        |
+----------------------+--------+------------+----------------------------------------------+
| Type                 |        | Kind       | Description                                  |
+----------------------+--------+------------+----------------------------------------------+
| List                 | ``::`` | ``* -> *`` | Polymorphic type or type constructor         |
+----------------------+--------+------------+----------------------------------------------+
| .. class:: center                                                                         |
|                                                                                           |
| Instances                                                                                 |
+----------------------+--------+------------+----------------------------------------------+
| List Int             | ``::`` | ``*``      | Concrete type (list of Ints)                 |
+----------------------+--------+------------+----------------------------------------------+
| List (Maybe Int)     | ``::`` | ``*``      | Concrete type (list of Maybe Ints)           |
+----------------------+--------+------------+----------------------------------------------+
| :strike:`List Maybe` | ``::`` |            | Invalid argument kind * -> *                 |
+----------------------+--------+------------+----------------------------------------------+

Value Constructors
..................

+--------------------------------------------------------------------------------------------------------+
| Return a value of a certain type by `creating` it or by `composing` argument values into a new value.  |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Value Constructor |        | Type                          | Description                               |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Empty             | ``::`` | List a                        | Create a new value (empty list)           |
+-------------------+--------+-------------------------------+-------------------------------------------+
| Cons              | ``::`` | Cons :: a -> List a -> List a | Compose two values (`a` and `List a`)     |
+-------------------+--------+-------------------------------+-------------------------------------------+

Heap Representation
~~~~~~~~~~~~~~~~~~~
TODO: Memory representation of the type (i.e. a closure)
with pointers to the contained types. Diagrams for the List example.

Terminology
...........

+-----------+---------------------------------------+-------------+
| Sum       | data Bool = False | True              | Monomorphic |
+-----------+---------------------------------------+-------------+
| Product   | data Point = Point Int Int            | Monomorphic |
+-----------+---------------------------------------+-------------+
| Recursive | data List a = Empty | Cons a (List a) | Polymorphic |
+-----------+---------------------------------------+-------------+

GADT Syntax
~~~~~~~~~~~

+------------------------------------------------------------------+
| Haskell98 Syntax (Constructor return type is implicit and fixed) |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a = Empty | Cons a (List a)                           |
+------------------------------------------------------------------+
| GADT Syntax (Constructor return type is explicit and can vary)   |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data List a where                                               |
|    Empty :: List a                                               |
|    Cons  :: a -> List a -> List a                                |
+------------------------------------------------------------------+
| GADT constructor type variables are universally quantified       |
| (Same as in function signatures)                                 |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data T a where      -- 'a' has no scope                         |
|   T1,T2 :: b -> T b  -- forall b. b -> T b                       |
|   T3 :: T a          -- forall a. T a                            |
+------------------------------------------------------------------+
| GADT Type parameters have no scope                               |
| (You can even omit them and just use the kind)                   |
+------------------------------------------------------------------+
| ::                                                               |
|                                                                  |
|  data Bar a b where ...                                          |
|  data Bar :: * -> * -> * where ...                               |
|  data Bar a :: ( * -> * ) where ...                              |
|  data Bar a ( b :: * -> * ) where ...                            |
+------------------------------------------------------------------+

GADT Semantics
~~~~~~~~~~~~~~

+-------------------------------------------------+----------------------------------------------------+
| -XGADTs                                                                                              |
+-------------------------------------------------+----------------------------------------------------+
| Ordinary type                                   | Generalized type (GADT)                            |
+-------------------------------------------------+----------------------------------------------------+
| One type represented by only one type level term| One type represented by multiple type level terms  |
+-------------------------------------------------+----------------------------------------------------+
| List Int                                        | Term Int                                           |
|                                                 +----------------------------------------------------+
|                                                 | Term Bool                                          |
|                                                 +----------------------------------------------------+
|                                                 | Term a                                             |
|                                                 +----------------------------------------------------+
|                                                 | Term (a,b)                                         |
+-------------------------------------------------+----------------------------------------------------+
| Return type of all the constructors same        | Each constructor can have a different return type  |
+-------------------------------------------------+----------------------------------------------------+

+-------------------------------------------------------+
| GADT Example                                          |
+-------------------------------------------------------+
| ::                                                    |
|                                                       |
|   data Term a where                                   |
|     Lit    :: Int -> Term Int                         |
|     Succ   :: Term Int -> Term Int                    |
|     IsZero :: Term Int -> Term Bool                   |
|     If     :: Term Bool -> Term a -> Term a -> Term a |
|     Pair   :: Term a -> Term b -> Term (a,b)          |
+-------------------------------------------------------+
| `deriving` clause cannot be used                      |
+-------------------------------------------------------+

+---------------------------------------------------------------+
| Pattern matching causes type refinement `based on signature`. |
| e.g. in `(Lit i)` `a` is refined to Int                       |
+---------------------------------------------------------------+
| ::                                                            |
|                                                               |
|  eval :: Term a -> a                                          |
|  eval (Lit i)      = i                                        |
|  eval (Succ t)     = 1 + eval t                               |
|  eval (IsZero t)   = eval t == 0                              |
|  eval (If b e1 e2) = if eval b then eval e1 else eval e2      |
|  eval (Pair e1 e2) = (eval e1, eval e2)                       |
+---------------------------------------------------------------+
| The following types must be rigid                             |
| (i.e. annotated by programmer) in a pattern match:            |
|                                                               |
| * scrutinee                                                   |
| * entire case expression                                      |
| * free variable mentioned in any of the case alternatives     |
+---------------------------------------------------------------+

Detailed Data Construction Syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
+------------------------------------------------------------+-------------------------------------------------------+
| .. class :: center                                                                                                 |
|                                                                                                                    |
| Records                                                                                                            |
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                         | ::                                                    |
|                                                            |                                                       |
|  data Person =                                             |   data Person where                                   |
|    Show a => Adult {                                       |     Adult :: Show a => {                              |
|        name     :: String                                  |         name     :: String                            |
|      , funny    :: a                                       |       , funny    :: a                                 |
|    }                                                       |       } -> Person                                     |
+------------------------------------------------------------+-------------------------------------------------------+
| Selector functions to extract a field from a record data structure are automatically generated for each record     |
| field::                                                                                                            |
|                                                                                                                    |
|  name    :: Person -> String                                                                                       |
|  funny   :: Person -> a                                                                                            |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| -XExistentialQuantification                                                                                        |
+------------------------------------------------------------+-------------------------------------------------------+
| Quantified type variables that appear in arguments but not in the result type for any constructor are existentials.|
| The type of any such variable cannot be checked against any type outside the bindings within this data type.       |
| So data Foo = forall a. Foo a (a -> a) is equivalent to Foo :: (exists a . (a, a -> a)) -> Foo.                    |
| It allows us to pack opaque data and operations on it together in a data type. An example using records:           |
+------------------------------------------------------------+-------------------------------------------------------+
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
| * Fields using existentials are `private`. They will not get a selector function and cannot be updated             |
| * Pattern matches to extract existentials are allowed only in `case` or `function definition` and not in `let` or  |
|   `where` bindings                                                                                                 |
| * As expected constraint is available on pattern match: ``f NewCounter {_this, _inc} = show (_inc _this)``         |
+------------------------------------------------------------+-------------------------------------------------------+
| .. class:: center                                                                                                  |
|                                                                                                                    |
| Strictness Annotations                                                                                             |
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
+------------------------------------------------------------+-------------------------------------------------------+
| ::                                                                                                                 |
|                                                                                                                    |
|  data T a    -- T :: * -> *                                                                                        |
+------------------------------------------------------------+-------------------------------------------------------+


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

Deconstruction (Pattern Matching)
---------------------------------

* TBD define scrutinee

+-----------------------------------------------------------------------------+
| Pattern matching is the only way to break down constructed data             |
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
| ::                                                                          |
|                                                                             |
|  let Cons x xs = list                                                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  where Cons x xs = list                                                     |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  case list of                                                               |
|    Cons x xs -> ...                                                         |
|    Empty     -> ...                                                         |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   f (Cons x xs) = ...                                                       |
|   f (Empty)     = ...                                                       |
|                                                                             |
|   f list -- apply the function to a list                                    |
+-----------------------------------------------------------------------------+

Lazy vs strict pattern match.

+-----------------------------------------------------------------------------+
| -XPatternGuards: write guards as pattern matches                            |
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
| -XViewPatterns: Pattern match on the result of an expression within a       |
| pattern match                                                               |
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

Type Synonyms
-------------

+-----------------------------------------------------------------------------+
| Create a type synonym for an existing type                                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type ThisOrThat a b = Either a b                                           |
|  type ThisOrInt  a   = Either a Int                                         |
+-----------------------------------------------------------------------------+
| The synonym can be used anywhere the original type can be used.             |
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
|   does not combine multiple types.                                          |
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
| `type` creates a `synonym` which means it can be freely used in place of the|
| original type and vice versa.  Both the types are swappable. However, the   |
| type created by `newtype` is an entirely new type and cannot be used in     |
| place of any other type.                                                    |
+-----------------------------------------------------------------------------+

Data Families
~~~~~~~~~~~~~

+----------------------------------------------------------------------+
| Polymorphic Types                                                    |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data List a = Empty | Cons a (List a)                               |
+----------------------------------------------------------------------+
| Every type instance uses the same constructor definition template    |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  List Char  -- Empty | Cons Char (List Char)                         |
|  List ()    -- Empty | Cons () (List ())                             |
+----------------------------------------------------------------------+

+----------------------------------------------------------------------+
| Data Family Prototype                                                |
| (declares the kind signature of the type function)                   |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data family List a                                                  |
|  data family List a :: *                                             |
|  data family List :: * -> *                                          |
+----------------------------------------------------------------------+
| Data Family Instances                                                |
| (define the type constructor function for each argument type)        |
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
| * The number of parameters of an instance declaration must match     |
|   the arity determined by the kind of the family.                    |
| * Overlap of instance declarations is not allowed                    |
| * You can use a deriving clause on a data instance or newtype        |
|   instance declaration                                               |
|                                                                      |
| Type parameters may not contain:                                     |
|                                                                      |
| * forall types                                                       |
| * type synonym families                                              |
| * partially applied type synonyms                                    |
| * fully applied type synonyms expanding to inadmissible types        |
+----------------------------------------------------------------------+

Type Synonym Families
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Open families (open to extension by adding instances)                       |
+-----------------------------------------------------------------------------+
| Declare the kind signature:                                                 |
+-----------------------------------------------------------------------------+
| The number of parameters in a type family declaration, is the family’s      |
| arity. The kind of a type family is not sufficient to determine a family’s  |
| arity. So we cannot use just the kind signature in declaration like we can  |
| in data families.                                                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family Elem c          -- Family Arity 1, Elem :: * -> *              |
|  type family Elem c :: *     -- Family Arity 1, Elem :: * -> *              |
|  type family F a b :: * -> * -- Family Arity 2, F :: * -> * -> * -> *       |
|  type family F a :: k        -- Poly kinded, k is an implicit parameter     |
+-----------------------------------------------------------------------------+
| Define instances:                                                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type instance Elem [e] = e                                                 |
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
| Applications: must be fully saturated with respect to the family arity      |
+----------------------------------+------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F a b :: * -> *  -- Family Arity 2, F :: * -> * -> * -> *      |
|  F Char [Int]                 -- OK!  Kind: * -> *                          |
|  F Char [Int] Bool            -- OK!  Kind: *                               |
|  F IO Bool                    -- WRONG: kind mismatch in the first argument |
|  F Bool                       -- WRONG: unsaturated application             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F a :: *                                                       |
|  type instance F (F a)   = a -- WRONG: type parameter mentions a type family|
|  type instance                                                              |
|    F (forall a. (a, b))  = b -- WRONG: a forall appears in a type parameter |
|  type instance                                                              |
|    F Float = forall a.a      -- WRONG: right-hand side may not be a forall  |
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
