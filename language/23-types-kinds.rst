Kinds
=====

Kinds and types are essentially the same except that they have a different
namespaces.

Kinds: Ensuring correctness of Types
------------------------------------

+-----------------------------------------------------------------------------+
| Safety of type level programming is ensured by labeling types with different|
| `kinds` and performing a `kind check` when a type function is applied.      |
| Kinds are relatively few and classified as follows:                         |
+-----------------------------------------------------------------------------+

Primitive Kinds
~~~~~~~~~~~~~~~

.. _RuntimeRep: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#runtime-representation-polymorphism

+--------------------------------------------------+--------------------------+
| Kind                                             | Description              |
+==================================================+==========================+
| ``TYPE 'IntRep'``, ``TYPE 'DoubleRep'`` ...      | Unlifted Types           |
+--------------------------------------------------+--------------------------+
| ``Type`` or ``*`` (``TYPE 'PtrRepLifted'``)      | Lifted Types             |
+--------------------------------------------------+--------------------------+
| ``Constraint``                                   | Typeclass Constraints    |
+--------------------------------------------------+--------------------------+
| ``Nat``                                          | Type level naturals      |
+--------------------------------------------------+--------------------------+
| ``Symbol``                                       | Type level symbols       |
+--------------------------------------------------+--------------------------+
| A concrete type's kind encodes the runtime representation (e.g. unlifted or |
| lifted) of the type.                                                        |
+-----------------------------------------------------------------------------+
| GHC internally represents a type kind as ``TYPE`` parameterised by          |
| `RuntimeRep`_.                                                              |
+-----------------------------------------------------------------------------+

Kind Signatures
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Just like a type signature associates types to a value, a `kind signature`  |
| associates kinds to a type.                                                 |
+-----------------------------------------------------------------------------+

Kinds of Concrete Types
^^^^^^^^^^^^^^^^^^^^^^^

+-----------+------+-------------------+
| Type      |      | Kind              |
+===========+======+===================+
| .. class:: center                    |
|                                      |
| Unlifted Types                       |
+-----------+------+-------------------+
| Int#      | `::` | TYPE 'IntRep'     |
+-----------+------+-------------------+
| Double#   | `::` | TYPE 'DoubleRep'  |
+-----------+------+-------------------+
| Array#    | `::` | TYPE 'ArrayRep'   |
+-----------+------+-------------------+
| .. class:: center                    |
|                                      |
| Lifted Types                         |
+-----------+------+-------------------+
| RealWorld | `::` | Type              |
+-----------+------+-------------------+
| Int       | `::` | Type              |
+-----------+------+-------------------+
| Maybe Int | `::` | Type              |
+-----------+------+-------------------+

Kinds of Type Functions
^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| We generate the kinds of type functions by using the kind level operator    |
| ``->``.                                                                     |
+-----------------------------------------------------------------------------+

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

+-----------------------------------------------------------------------------+
| `Kind check` fails if we pass the wrong kind to a type function.            |
+------------------------------+-------------+--------------------------------+
| Function                     | Application | Failure Reason                 |
+------------------------------+-------------+--------------------------------+
| Maybe :: Type -> Type        | Maybe Int#  | Wrong kind ``TYPE 'IntRep'``   |
|                              |             | expected ``Type``              |
+------------------------------+-------------+                                |
| (->) :: Type -> Type -> Type | Int# -> Int |                                |
+------------------------------+-------------+--------------------------------+

The Kind *
----------

The kind ``*`` classifies ordinary types. The ``Data.Kind`` module also exports
``Type`` as a synonym for ``*``.  GHC may deprecate * in future.

Without -XPolyKinds or -XTypeInType enabled, GHC refuses to generalise over
kind variables. It thus defaults kind variables to * when possible.

Without -XTypeInType, this identifier is always in scope when writing a kind.
However, with -XTypeInType, a user may wish to use * in a type or a type
operator * in a kind. To make this all more manageable, * becomes an (almost)
ordinary name with -XTypeInType enabled. So as not to cause naming collisions,
it is not imported by default; you must import Data.Kind to get * (but only
with -XTypeInType enabled).

+-----------------------------------------------------------------------------+
| Parsing of * is special and there are some corner cases that are not        |
| handled. For example,:                                                      |
+-----------------------------------------------------------------------------+
| In a Haskell-98-style data constructor or in an import/export list, you     |
| must put parentheses around ``*``:                                          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Universe = Ty (*) | Num Int | ...                                     |
|  import Data.Kind ( type (*) )                                              |
+-----------------------------------------------------------------------------+

DataKinds
---------

+-----------------------------------------------------------------------------+
| -XDataKinds                                                                 |
+-----------------------------+-----------------------------------------------+
| Type level                  | Kind and kind level type constructors         |
| User defined                | auto generated by GHC                         |
+-----------------------------+-----------------------------------------------+
| ::                          | ::                                            |
|                             |                                               |
|  data Nat = Zero | Succ Nat |  data Nat = 'Zero | 'Succ Nat                 |
|                             |                                               |
|                             |  Nat :: *                                     |
|                             |  'Zero :: Nat                                 |
|                             |  'Succ :: Nat -> Nat                          |
+-----------------------------+-----------------------------------------------+
| ::                          | ::                                            |
|                             |                                               |
|  data Pair a b = Pair a b   |  data Pair a b = 'Pair a b                    |
|                             |                                               |
|                             |  Pair  :: * -> * -> *                         |
|                             |  'Pair :: forall k1 k2. k1 -> k2 -> Pair k1 k2|
+-----------------------------+-----------------------------------------------+
| ::                          | ::                                            |
|                             |                                               |
|  data Sum a b = L a | R b   |  data Sum a b = 'L a | 'R b                   |
|                             |                                               |
|                             |  Sum :: * -> * -> *                           |
|                             |  'L :: k1 -> Sum k1 k2                        |
|                             |  'R :: k2 -> Sum k1 k2                        |
+-----------------------------+-----------------------------------------------+
| Existential constructors get promoted as usual.                             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Ex :: * where                                                         |
|    MkEx :: forall a. a -> Ex                                                |
+-----------------------------------------------------------------------------+
| You can write a type family to extract the member of a type-level           |
| existential:                                                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family UnEx (ex :: Ex) :: k                                           |
|  type instance UnEx (MkEx x) = x                                            |
+-----------------------------------------------------------------------------+
| Only types with kind * are inhabited, so the types introduced by            |
| ``DataKinds`` are not inhabited.                                            |
+-----------------------------------------------------------------------------+
| All promoted constructors are prefixed with a single quote mark \'. This    |
| mark tells GHC to look in the data constructor namespace for a name, not    |
| the type (constructor) namespace.                                           |
+-----------------------------------------------------------------------------+
| GHC allows you to omit the quote mark when the name is unambiguous.         |
+-----------------------------------------------------------------------------+
| To avoid ambiguity with character literals use a space after the quote mark.|
| For example, write ``' A'`` instead of ``'A'``.                             |
+-----------------------------------------------------------------------------+
| ``-Wunticked-promoted-constructors`` warns you if you use a promoted        |
| data constructor without a preceding quote mark.                            |
+-----------------------------------------------------------------------------+
| With ``-XDataKinds``, Haskell’s list and tuple types are natively promoted  |
| to kinds, and enjoy the same convenient syntax at the type level, albeit    |
| prefixed with a quote e.g. ``'[]``, ``'[Int]`` or ``':``. When there are    |
| more than one items in the list the quote can be omitted since there is no  |
| ambiguity e.g. ``[Int, Bool]``.                                             |
+-----------------------------------------------------------------------------+
| -XTypeInType                                                                |
+-----------------------------------------------------------------------------+
| * Promotion of type synonyms and type families, but not data families.      |
| * All datatypes, even those with rich kinds, get promoted.                  |
+-----------------------------------------------------------------------------+

Data Kinds Notes
----------------

::

  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE NoImplicitPrelude #-}

  import Data.Text
  import Data.Time
  import Prelude hiding (Read)

  data CrudAction = Read | Create | Update

  type family CrudHelper (crudAction :: CrudAction) read create update where
    CrudHelper Read read _ _ = read
    CrudHelper Create _ create _ = create
    CrudHelper Update _ _ update = update


  newtype UserId = UserId Int
  newtype Encrypted = Encrypted Text

  data User (crudAction :: CrudAction) = User
    {
      userKey :: CrudHelper crudAction UserId () ()
      , userName :: CrudHelper crudAction Text Text (Maybe Text)
      , userAge :: CrudHelper crudAction Int Int (Maybe Int)
      , userEmail :: CrudHelper crudAction Text Text ()
      , userPassword :: CrudHelper crudAction Encrypted Text ()
      , createdAt :: CrudHelper crudAction LocalTime () ()
      , updatedAt :: CrudHelper crudAction LocalTime () ()
    }

  type UserRead = User Read
  type UserCreate = User Create
  type UserUpdate = User Update

Kind Polymorphism and Kind Inference
------------------------------------

+-----------------------------------------------------------------------------+
| -XTypeInType and -XPolyKinds                                                |
| (-XTypeInType is a superset of -XPolyKinds)                                 |
+-----------------------------------------------------------------------------+
| When there is a right-hand side, GHC infers the most polymorphic kind       |
| consistent with the right-hand side.                                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- Kind of App with Haskell 98:  (* -> \*) -> * -> *                       |
|  -- Kind of App with -XPolyKinds: forall k. (k -> \*) -> k -> *             |
|  data App f a = MkApp (f a)                                                 |
|                                                                             |
|  App Maybe Int             -- `k` is instantiated to *                      |
|  data T a = MkT (a Int)    -- `a` is inferred to have kind (* -> \*)        |
|  App T Maybe               -- `k` is instantiated to (* -> \*)              |
+-----------------------------------------------------------------------------+
| When there is no right hand side, GHC defaults argument and result kinds to |
| ``*``, except when directed otherwise by a kind signature.                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F1 a                -- F1 :: * -> *                            |
|  type family F2 (a :: k)         -- F2 :: forall k. k -> *                  |
|  type family F3 a :: k           -- F3 :: forall k. * -> k                  |
|  type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2            |
+-----------------------------------------------------------------------------+
| These rules have occasionally-surprising consequences. The                  |
| kind-polymorphism from the following class declaration makes                |
| D1 kind-polymorphic, but not so D2; and similarly for F1, F2.               |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class C a where    -- Class declarations are generalised                   |
|                     -- so C :: forall k. k -> Constraint                    |
|    data D1 a        -- No right hand side for these two family              |
|    type F1 a        -- declarations, but the class forces (a :: k)          |
|                     -- so   D1, F1 :: forall k. k -> *                      |
|                                                                             |
|  data D2 a   -- No right-hand side so D2 :: * -> *                          |
|  type F2 a   -- No right-hand side so F2 :: * -> *                          |
+-----------------------------------------------------------------------------+
| Kind inference in class instance declarations                               |
+-----------------------------------------------------------------------------+
| GHC does not propagate kind information from the members of a class         |
| instance declaration into the instance declaration head.                    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class C a where                                                            |
|    type F a                                                                 |
|                                                                             |
|  instance C b where                                                         |
|    type F b = b -> b -- b's kind can be inferred as '*' but is not.         |
+-----------------------------------------------------------------------------+
| If you want to restrict b‘s kind in the instance above, just use a kind     |
| signature in the instance head.                                             |
+-----------------------------------------------------------------------------+

Levity polymorphism
-------------------


Polymorphic Recursion and Kind-Indexing
---------------------------------------

+-----------------------------------------------------------------------------+
| Polymorphic Recursion                                                       |
+-----------------------------------------------------------------------------+
| When a datatype is used at different kinds in its body it can be seen as    |
| a form of polymorphic recursion.                                            |
+-----------------------------------------------------------------------------+
| Just as in type inference, kind inference for recursive types can only use  |
| monomorphic recursion.                                                      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data T m a = MkT (m a) (T Maybe (m a))                                     |
|  -- GHC infers kind  T :: (* -> \*) -> * -> *                               |
+-----------------------------------------------------------------------------+
| However, just as in type inference, you can achieve polymorphic recursion   |
| by giving a complete user-supplied kind signature (or CUSK) for T.          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data T (m :: k -> \*) :: k -> * where                                      |
|    MkT :: m a -> T Maybe (m a) -> T m a                                     |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Kind-Indexing                                                               |
+-----------------------------------------------------------------------------+
| We can see kind-indexing as a form of polymorphic recursion, where a type   |
| is used at a kind other than its most general in its own definition.        |
| GHC will not infer this behaviour without a complete user-supplied kind     |
| signature, as doing so would sometimes infer non-principal types.           |
+-----------------------------------------------------------------------------+
| Kind-Indexed type families                                                  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family F1 a where                                                     |
|    F1 True  = False                                                         |
|    F1 False = True                                                          |
|    F1 x     = x                                                             |
|  -- F1 fails to compile: kind-indexing is not inferred                      |
|                                                                             |
|  type family F2 (a :: k) where                                              |
|    F2 True  = False                                                         |
|    F2 False = True                                                          |
|    F2 x     = x                                                             |
|  -- F2 fails to compile: no complete signature                              |
|                                                                             |
|  type family F3 (a :: k) :: k where                                         |
|    F3 True  = False                                                         |
|    F3 False = True                                                          |
|    F3 x     = x                                                             |
|  -- OK                                                                      |
+-----------------------------------------------------------------------------+
| Kind-Indexed GADTs                                                          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data G (a :: k) where                                                      |
|    GInt    :: G Int                                                         |
|    GMaybe  :: G Maybe                                                       |
+-----------------------------------------------------------------------------+
| Suppose you have ``g :: G a``, where ``a :: k``. Then pattern matching to   |
| discover that ``g`` is in fact ``GMaybe`` tells you both that               |
| ``k ~ (* -> \*)`` and ``a ~ Maybe``.                                        |
+-----------------------------------------------------------------------------+

Dependencies in Datatype Declarations
-------------------------------------

+-----------------------------------------------------------------------------+
| If a type variable ``a`` in a datatype, class, or type family declaration   |
| depends on another such variable ``k`` in the same declaration, then the    |
| following two rules must hold.                                              |
+-----------------------------------------------------------------------------+
| Scoping rule:                                                               |
| ``a`` must appear after ``k`` in the declaration                            |
+-----------------------------------------------------------------------------+
| Explicit dependency rule:                                                   |
| ``k`` must appear explicitly in the kind of some type variable in that      |
| declaration.                                                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Proxy k (a :: k)            -- OK: dependency is "obvious"            |
|  data Proxy2 k a = P (Proxy k a)  -- ERROR: dependency is unclear           |
+-----------------------------------------------------------------------------+

CUSK
----

+-----------------------------------------------------------------------------+
| Complete User Supplied Kind Signature (CUSK)                                |
+-----------------------------------------------------------------------------+
| A CUSK is present when all argument kinds and the result kind are known,    |
| without any need for inference.                                             |
+-----------------------------------------------------------------------------+
| For a datatype, every type variable must be annotated with a kind.          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data T1 :: (k -> \*) -> k -> *       where ...                             |
|  -- Yes;  T1 :: forall k. (k-> \*) -> k -> *                                |
|                                                                             |
|  data T2 (a :: k -> \*) :: k -> *     where ...                             |
|  -- Yes;  T2 :: forall k. (k-> \*) -> k -> *                                |
|                                                                             |
|  data T3 (a :: k -> \*) (b :: k) :: * where ...                             |
|  -- Yes;  T3 :: forall k. (k-> \*) -> k -> *                                |
|                                                                             |
|  data T4 (a :: k -> \*) (b :: k)      where ...                             |
|  -- Yes;  T4 :: forall k. (k-> \*) -> k -> *                                |
|                                                                             |
|  data T5 a (b :: k) :: *             where ...                              |
|  -- No;  kind is inferred                                                   |
|                                                                             |
|  data T6 a b                         where ...                              |
|  -- No;  kind is inferred                                                   |
+-----------------------------------------------------------------------------+
| For a datatype with a top-level :: when -XTypeInType is in effect: all kind |
| variables introduced after the :: must be explicitly quantified.            |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- -XTypeInType is on                                                      |
|  data T1 :: k -> *                   -- No CUSK: not explicitly quantified  |
|  data T2 :: forall k. k -> *         -- CUSK: `k` is bound explicitly       |
|  data T3 :: forall (k :: \*). k -> * -- CUSK                                |
+-----------------------------------------------------------------------------+
| For a class, every type variable must be annotated with a kind.             |
+-----------------------------------------------------------------------------+
| For a type synonym, every type variable and the result type must all be     |
| annotated with kinds:                                                       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type S1 (a :: k) = (a :: k)    -- Yes   S1 :: forall k. k -> k             |
|  type S2 (a :: k) = a           -- No    kind is inferred                   |
|  type S3 (a :: k) = Proxy a     -- No    kind is inferred                   |
+-----------------------------------------------------------------------------+
| An un-associated open type or data family declaration always has a CUSK;    |
| un-annotated type variables default to kind \*:                             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data family D1 a               -- D1 :: * -> *                             |
|  data family D2 (a :: k)        -- D2 :: forall k. k -> *                   |
|  data family D3 (a :: k) :: *   -- D3 :: forall k. k -> *                   |
|  type family S1 a :: k -> *     -- S1 :: forall k. * -> k -> *              |
+-----------------------------------------------------------------------------+
| An associated type or data family declaration has a CUSK precisely if its   |
| enclosing class has a CUSK.                                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class C a where                -- no CUSK                                  |
|    type AT a b                  -- no CUSK, b is defaulted                  |
|                                                                             |
|  class D (a :: k) where         -- yes CUSK                                 |
|    type AT2 a b                 -- yes CUSK, b is defaulted                 |
+-----------------------------------------------------------------------------+
| A closed type family has a complete signature when all of its type          |
| variables are annotated and a return kind (with a top-level ::) is supplied.|
+-----------------------------------------------------------------------------+
| With -XTypeInType enabled, it is possible to write a datatype that          |
| syntactically has a CUSK (according to the rules above) but actually        |
| requires some inference.                                                    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Proxy a           -- Proxy :: forall k. k -> *                        |
|  data X (a :: Proxy k)                                                      |
+-----------------------------------------------------------------------------+
| If you wish k to be polykinded, it is straightforward to specify this:      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data X (a :: Proxy (k1 :: k2))                                             |
+-----------------------------------------------------------------------------+

Quantification
--------------

+-----------------------------------------------------------------------------+
| Kind Quantification                                                         |
+-----------------------------------------------------------------------------+
| For backward compatibility, kind variables do not need to be bound          |
| explicitly, even if the type starts with ``forall``.                        |
+-----------------------------------------------------------------------------+
| When quantification is used, kind must be declared before use, for example  |
| ``forall (a :: k) k.`` is an error.                                         |
| All kind variables mentioned in a type are bound at the outermost level.    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Proxy :: forall k. k -> *                                             |
|  f :: (forall k (a :: k). Proxy a -> ()) -> Int                             |
+-----------------------------------------------------------------------------+
| In GHC 7, if a kind variable was mentioned for the first time in the kind   |
| of a variable bound in a non-top-level forall, the kind variable was bound  |
| there, too. That is, in f :: (forall (a :: k). ...) -> ..., the k was bound |
| by the same forall as the a.                                                |
+-----------------------------------------------------------------------------+

Kind Constraints
----------------

+-----------------------------------------------------------------------------+
| ``-XTypeInType`` allows kinds to contain type constraints. Only equality    |
| constraints are currently supported.                                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  type family IsTypeLit a where                                              |
|    IsTypeLit Nat    = 'True                                                 |
|    IsTypeLit Symbol = 'True                                                 |
|    IsTypeLit a      = 'False                                                |
|                                                                             |
|  data T :: forall a. (IsTypeLit a ~ 'True) => a -> * where                  |
|    MkNat    :: T 42                                                         |
|    MkSymbol :: T "Don't panic!"                                             |
+-----------------------------------------------------------------------------+

Type-Level Literals
-------------------

* GHC.TypeLits
* GHC.TypeNats

References
----------

* https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell
* http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
* http://dreixel.net/research/pdf/ghp.pdf Giving Haskell A Promotion

