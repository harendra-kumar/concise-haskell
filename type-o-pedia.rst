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
* associated types

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
| Polymorphic                | Has multiple possible representation (cannnot be concrete)      |
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
| Universal Quantification   | forall `a`                                                      |
+----------------------------+-----------------------------------------------------------------+
| Existential Quantification | exists `a`                                                      |
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

+-------------+--------+----------------------+--------------------------------------------------------------------------------+
| Kinds                | Lifted Types         | ``*``                                                                          |
|                      +----------------------+--------------------------------------------------------------------------------+
|                      | Unlifted Types       | ``TYPE 'IntRep'``, ...                                                         |
+-------------+--------+----------------------+--------------------------------------------------------------------------------+
| Types       | Rank1  | Polymorphic Type Fns | ``t :: k1 -> k2``, where k1, k2 are kind variables representing types of rank0 |
|             +--------+----------------------+--------------------------------------------------------------------------------+
|             | Rank0  | Type Functions       | ``t :: * -> *`` (polymorphic type)                                             |
|             |        +----------------------+--------------------------------------------------------------------------------+
|             |        | Concrete Types       | ``t :: *``                                                                     |
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
| Data Family Prototype                                                |
| (declares the kind signature of the type function)                   |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data family List a -- List :: * -> *                                |
+----------------------------------------------------------------------+
| Data Family Instances                                                |
| (define the type constructor function for each argument type)        |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data instance List Char = Empty | Cons Char (List Char)             |
|  data instance List ()   = Count Int                                 |
+----------------------------------------------------------------------+

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
|  data Bar a :: (* -> \*) where ...                               |
|  data Bar a (b :: * -> \*) where ...                             |
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
| data NumInst a = Num a => MkNumInst                        | | data NumInst a where                                |
|                                                            | |  MkNumInst :: Num a => NumInst a                    |
+------------------------------------------------------------+-------------------------------------------------------+
| ``MkNumInst`` reifies ``Num`` dictionary: plus :: NumInst a -> a -> a -> a; plus MkNumInst p q = p + q             |
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
| constructor is broken down into its components. We write a constructor      |
| pattern on the LHS of an equation and the composed data structure on the    |
| RHS. If the pattern matches with the data structure then the variables in   |
| the pattern are assigned the individual pieces of the data structure.       |
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

Type Synonym Families
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Open families                                                               |
+-----------------------------------------------------------------------------+
| type family Elem c :: *     | Arity 1, Kind * -> *                          |
+-----------------------------------------------------------------------------+
type instance Elem [e] = e    |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Instances: cannot have conflicting LHS and RHS in instance equations        |
+-----------------------------------------------------------------------------+
type instance F (a, Int) = [a] | Compatible overlap, allowed.
type instance F (Int, b) = [b] |
+-----------------------------------------------------------------------------+
type instance G (a, Int)  = [a] | Conflicting overlap, as [Char] /= [Int].
type instance G (Char, a) = [a] | Not allowed.
+-----------------------------------------------------------------------------+
type instance H x   x = Int   | Conflicting overlap when x is infinite nesting
type instance H [x] x = Bool  | of lists. Not allowed.
+-----------------------------------------------------------------------------+

* all applications of a type family must be fully saturated with respect to to that arity
+-----------------------------------------------------------------------------+
| type family F a b :: * -> * | Arity 2, Kind * -> * -> * -> *                |

F Char [Int]       -- OK!  Kind: * -> *
F Char [Int] Bool  -- OK!  Kind: *
F IO Bool          -- WRONG: kind mismatch in the first argument
F Bool             -- WRONG: unsaturated application

* Poly-kinded
type family F a :: k
* the kind parameter k is actually an implicit parameter of the type family

For a polykinded type family, the kinds are checked for apartness just like types. For example, the following is accepted:

type family J a :: k
type instance J Int = Bool
type instance J Int = Maybe
These instances are compatible because they differ in their implicit kind parameter; the first uses * while the second uses * -> \*.

* closed families

type family F a where
  F Int  = Double
  F Bool = Char
  F a    = String

* A closed type family’s equations are tried in order, from top to bottom

type family F a :: *
type instance F [Int]   = Int   -- OK!
type instance F String  = Char  -- OK!
type instance F (F a)   = a     -- WRONG: type parameter mentions a type family
type instance
  F (forall a. (a, b))  = b     -- WRONG: a forall type appears in a type parameter
type instance
  F Float = forall a.a          -- WRONG: right-hand side may not be a forall type
type family H a where          -- OK!
  H Int  = Int
  H Bool = Bool
  H a    = String
type instance H Char = Char    -- WRONG: cannot have instances of closed family
type family K a where          -- OK!

type family G a b :: * -> *
type instance G Int            = (,)     -- WRONG: must be two type parameters
type instance G Int Char Float = Double  -- WRONG: must be two type parameters

F a does not simplify. F Double simplifies to Char:
type family F a where
  F Int = Bool
  F a   = Char

Two equations are fully compatible and the first one can be ignored, G a
simplifies to a:
type family G a where
  G Int = Int
  G a   = a

-XUndeciableInstances: allow undecidable type synonym instances.

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

Data types

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

Type synonyms
