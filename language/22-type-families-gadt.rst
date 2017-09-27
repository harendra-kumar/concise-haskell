.. raw:: html

  <style> .red {color:red} </style>
  <style> .blk {color:black} </style>
  <style> .center { text-align: center;} </style>
  <style> .strike { text-decoration: line-through;} </style>

.. role:: strike
.. role:: center

.. role:: red
.. role:: blk

Generalized Type Polymorphism
-----------------------------

+------------------------------------------------+-----+-------------------------------------------------------------------+
| .. class:: center                              |     | .. class:: center                                                 |
|                                                |     |                                                                   |
| Type Level Function                            |     | Data Constructor Templates                                        |
+=========+=====================+================+=====+=====================+=======+=====================================+
|         | Type Constructor    |      Parameter |     | Data Constructor    |       | Data Constructor                    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| data    | :red:`L`:blk:`ist`  | `a`            |  =  | :red:`E`:blk:`mpty` | ``|`` | :red:`C`:blk:`ons`  a   (List a)    |
+---------+---------------------+----------------+-----+---------------------+-------+-------------------------------------+
| .. class:: center                              |     | .. class:: center                                                 |
|                                                |     |                                                                   |
| Generalization: data families                  |     | Generalization: GADTs                                             |
+------------------------------------------------+-----+-------------------------------------------------------------------+

Generalized Algebraic Data Types (GADTs)
----------------------------------------

+-----------------------------------------------------------------------------+
| -XGADTs                                                                     |
+-----------------------------------------------------------------------------+

The traditional ADTs are rigid in polymorphism, as the type is fixed
irrespective of the value constructor, or in other words all data constructors have
the same type i.e. the type of the data type. We will call them monolithic
polymorphic types.

In a GADT the type of individual data constructors can be different and the
datatype can be thought of as an aggregation (contrast with monolithic) of all
those constituent types. This can be compared to the pattern matched or case
analyzed definition of a function, the way a function is defined for each
case, here the type is defined for each case. For this reason GADTs do not make
sense for purely product types where there is only one data constructor. Also
GADTs do not make sense for monomorphic types as it has to be a polymorphic type
for different constructors to have a different type.

In other words, a constituent type is a function of or dependent on the
specific data constructor thus providing us dependent types. The constituent
type is retrieved along with the value on a pattern match on the value
constructor. This retrieved type can be unified at the type level i.e. via the
type signatures thus controlling the type level logic based on value level
information.

+-----------------------------------------------------------------------------+
| Representing terms in an expression with static typechecking.               |
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
| The aggregated type ``Term a`` represents the type of any term i.e. an      |
| abstraction for the aggregation of the return types of all constructors of  |
| this data type even though individual terms have different types.           |
+-----------------------------------------------------------------------------+

+--------------------------------------------------------------------------------+
| The type of an evaluated expression depends on the specific expression         |
| being evaluated and is determined by inference.                                |
+--------------------------------------------------------------------------------+
| ::                                                                             |
|                                                                                |
|    eval :: Term a -> a                                                         |
|                                                                                |
|    eval (Lit 10)                                                 -- Int        |
|    eval (Succ (Lit 10))                                          -- Int        |
|    eval (IsZero (Lit 10))                                        -- Bool       |
|    eval (If (IsZero (Lit 10)) (Lit 0) (Lit 1))                   -- Int        |
|    eval (If (IsZero (Lit 10)) (IsZero (Lit 0)) (IsZero (Lit 1))) -- Bool       |
|    eval (Pair (Lit 10) (Lit 20))                                 -- (Int, Int) |
+--------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| An expression is represented by a data type which is a collection or sum    |
| of terms in that expression.                                                |
+-----------------------------------------------------------------------------+
| Since each expression evaluates to a different type `we need what that type |
| is for each expression`. `We also need a way to somehow propagate this type |
| information and use it when we evaluate the expression`.                    |
+-----------------------------------------------------------------------------+
| The type information for each expression is encoded as the return type of   |
| the constructor e.g. ``Term Bool`` return type means the expression         |
| evaluates to a ``Bool`` value.                                              |
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
|                                                                             |
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
| A monolithic polymorphic type vs an aggregated type (GADT).                                          |
+-------------------------------------------------+----------------------------------------------------+
| A monolithic polymorphic type                   | Aggregated polymorphic type (GADT)                 |
+-------------------------------------------------+----------------------------------------------------+
| All constructors return the same type           | One or more constructors return a concrete type    |
| parameterized by a type variable.               | instance (e.g. Term Int).                          |
+-------------------------------------------------+----------------------------------------------------+
| Defines an abstract type e.g. ``List``.         | Defines the sum type as a group of concrete type   |
|                                                 | instances.                                         |
+-------------------------------------------------+----------------------------------------------------+
| We `instantiate` ``List`` to create concrete    | We `abstract` the group of concrete types          |
| type instances.                                 | to a polymorphic type ``Term a``.                  |
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
| Note that a GADT definition allows both the components described above i.e. an                       |
| external type parameter (as in the case of monolithic polymorphic types) as                          |
| well as a parameter representing the aggregated constituents. Therefore the                          |
| traditional ADTs are a special case of GADTs.                                                        |
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

+-----------------------------------------------------------------------------+
| If you like Existentially quantified types, you'd probably want to notice   |
| that they are now subsumed by GADTs. As the GHC manual says, the following  |
| two type declarations give you the same thing.                              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data TE a = forall b. MkTE b (b->a)                                        |
|  data TG a where { MkTG :: b -> (b->a) -> TG a }                            |
+-----------------------------------------------------------------------------+

Data Families
-------------

+----------------------------------------------------------------------+
| A polymorphic type is a `total` type function; it defines data       |
| constructors for all possible values of the type parameter.          |
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
| A data family is a `partial` type function defined using             |
| `pattern match` definitions, for the members of the family. The      |
| function is open to extension as new instances can be defined later. |
+----------------------------------------------------------------------+
| A data family `prototype` declares the kind signature of the type    |
| function. All of the following declarations are equivalent:          |
+----------------------------------------------------------------------+
| ::                                                                   |
|                                                                      |
|  data family List a                                                  |
|  data family List a :: Type                                          |
|  data family List   :: Type -> Type                                  |
+----------------------------------------------------------------------+
| A `data instance` defines the type function for specific values of   |
| its parameters (`a` in the above example) known as members of the    |
| family.                                                              |
+----------------------------------------------------------------------+
| A data instance can be compared to a function definition using       |
| pattern match. A pattern match extracts the constituent types of a   |
| member type and they can be used in the RHS of the instance:         |
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
---------------------

+-------------------------------------------------------------------------------------+
| Open families (open to extension by adding instances)                               |
+-------------------------------------------------------------------------------------+
| Declare a type family by specifying the kind signature:                             |
+-------------------------------------------------------------------------------------+
| The kind of a type family is not sufficient to determine its arity.                 |
| So unlike data families, we cannot use just the kind                                |
| signature in the declaration.                                                       |
+-------------------------------------------------------------------------------------+
| ::                                                                                  |
|                                                                                     |
|  type family F1 c                    -- Arity 1, F1 :: Type -> Type                 |
|  type family F1 c    :: Type         -- Arity 1, F1 :: Type -> Type                 |
|  type family F2 a b  :: Type -> Type -- Arity 2, F2 :: Type -> Type -> Type -> Type |
+-------------------------------------------------------------------------------------+
| Poly kinded or kind-indexed type families where the family matches both on the kind |
| and type. The kind is passed as an implicit kind parameter in this case.            |
+-------------------------------------------------------------------------------------+
| ::                                                                                  |
|                                                                                     |
|  type family F3 a    :: k            -- Poly kinded, k is an implicit parameter     |
+-------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Defining instances:                                                         |
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
| Creating an instance of a closed family results in an error                 |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| `-XUndeciableInstances`: allow undecidable type synonym instances.          |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| -XTypeFamilyDependencies                                                    |
+-----------------------------------------------------------------------------+
| Define injective type families by functional dependency annotations         |
+----------------------------------+------------------------------------------+
| ::                               | ::                                       |
|                                  |                                          |
|  type family Id a                |  id :: Id t -> Id t                      |
|  type instance Id Int = Int      |  id x = x                                |
|  type instance Id Bool = Bool    |                                          |
+----------------------------------+------------------------------------------+
| Type variable t appears only under type family applications and is thus     |
| ambiguous to inferencer. A functional dependency removes the ambiguity.     |
+-----------------------------------------------------------------------------+
| type family Id a = r | r -> a                                               |
+-----------------------------------------------------------------------------+

References
----------

* https://wiki.haskell.org/GADTs_for_dummies
* http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf Fun with phantom types.
* http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity_extended.pdf
* https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell
* https://akaposi.github.io/away_day/balestrieri.pdf Gadt: Almost Dependent Types
