+------------------------+----------------------------------------------------+
| Monotype               | A monomorphic type                                 |
+------------------------+----------------------------------------------------+
| Kind                   | Type of types (e.g. a type could be lifted or      |
|                        | unlifted)                                          |
+------------------------+----------------------------------------------------+
| Universal              |                                                    |
| quantification         |                                                    |
+------------------------+----------------------------------------------------+
| Existential            |                                                    |
| quantification         |                                                    |
+------------------------+----------------------------------------------------+

Parametric Polymorphism
-----------------------

When the parameters of a function are of a variable type i.e. polymorphic then
the function is known as parametrically polymorphic function.

Polymorphic Types
~~~~~~~~~~~~~~~~~

A parametrically polymorphic type is a type function parameterized by a type
variable (``a`` in the following example)::

  data Pair a = Pair a a

The type can be `instantiated` for a specific value of the variable `a`, for
example the type ``Pair Int`` is equivalent to the definition ``Pair Int Int``.

Polymorphic Functions
~~~~~~~~~~~~~~~~~~~~~

The arguments and/or return value of a parametrically polymorphic function can
be a variable type. The function can be `instantiated` for any value of the
type variable::

  id :: a -> a
  id x = x

The `a` in the signature of this function is a `type variable`. `a` can assume
any concrete type.

Scope and Quantification of Type Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type variables in a type signature lexically scope over the whole type
signature. However, unless ``ScopedTypeVariables`` is enabled, they are not
visible to any type signatures inside the declararion.

The type variables in a function signature are by default `universally
quantified`. You can think of quantification as scoping from the typechecker
perspective. Universal quantification implies that the type variables are
scoped globally across the entire program from the typechecker perspective.
Therefore, when a polymorphic function is `instantiated`, the specific values
of the type variables are determined by the user of the function.  For
example::

  let x = 'a'
  id x -- id :: Char -> Char, because x is of type Char

When a (universally quantified) type variable occurs at more than one places in
a signature it means that both the types are same. For example the argument and
the result type in the following function must be the same::

  id :: a -> a
  id :: Int -> Int
  id :: Char -> Char

+-----------------------------------------------------------------------------+
| A programmer-written type signature is implicitly quantified over its free  |
| type variables.                                                             |
+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XExplicitForAll                                                           |
+-----------------------------------------------------------------------------+
| Allow use of `forall` keyword where universal quantification is implicit.   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  id :: forall a. a -> a                                                     |
|  id :: forall a. (a -> a)                                                   |
|  instance forall a. Eq a => Eq [a] where ...                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XScopedTypeVariables                                                      |
+-----------------------------------------------------------------------------+
| Enable lexical scoping of type variables explicitly introduced with         |
| `forall`. `The type variables bound by a forall` scope over the entire      |
| definition of the accompanying value declaration.                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. [a] -> [a]                                                  |
|  f xs = ys ++ ys                                                            |
|      where                                                                  |
|        ys :: [a]                                                            |
|        ys = reverse xs                                                      |
+-----------------------------------------------------------------------------+
| * A scoped type variable stands for a type variable, and not for a type.    |
| * Distinct lexical type variables stand for distinct type variables         |
| * A lexically scoped type variable can be bound by a declaration,           |
|   expression, pattern type signature and class and instance declarations.   |
+-----------------------------------------------------------------------------+
| * Any type variable that is `in scope` is not universally quantified.       |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  (e :: a -> a)     means     (e :: a -> a)                                  |
|  (e :: b -> b)     means     (e :: forall b. b->b)                          |
+-----------------------------------------------------------------------------+
| An expression type signature that has explicit quantification               |
| (using forall) brings into scope the explicitly-quantified type variables,  |
| in the annotated expression. For example:                                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f = runST ( (op >>= \(x :: STRef s Int) -> g x) :: forall s. ST s Bool )   |
+-----------------------------------------------------------------------------+
| Unlike expression and declaration type signatures, pattern type signatures  |
| are not implicitly generalised. The pattern in a pattern binding may only   |
| mention type variables that are already in scope. For example:              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. [a] -> (Int, [a])                                           |
|  f xs = (n, zs)                                                             |
|    where                                                                    |
|      (ys::[a], n) = (reverse xs, length xs) -- OK                           |
|      zs::[a] = xs ++ ys                     -- OK                           |
|                                                                             |
|      Just (v::b) = ...  -- Not OK; b is not in scope                        |
+-----------------------------------------------------------------------------+
| However, in all patterns other than pattern bindings, a pattern type        |
| signature may mention a type variable that is not in scope; in this case,   |
| the signature brings that type variable into scope. This is particularly    |
| important for existential data constructors. For example:                   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data T = forall a. MkT [a]                                                 |
|                                                                             |
|  k :: T -> T                                                                |
|  k (MkT [t::a]) =                                                           |
|      MkT t3                                                                 |
|    where                                                                    |
|      t3::[a] = [t,t,t]                                                      |
+-----------------------------------------------------------------------------+
| in this situation (and only then), a pattern type signature can mention a   |
| type variable that is not already in scope; the effect is to bring it       |
| into scope, standing for the existentially-bound type variable.             |
+-----------------------------------------------------------------------------+

Higher Rank Parametric Polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When all the type variables of a function are universally quantified the values
of type variables and therefore the function instance is completely decided by
the caller context. However, when one of the parameters of a polymorphic
function is a function, that function will be called by the polymorphic
function itself. If that function is polymorphic and its type variables are not
universally quantified but scoped to the polymorphic function itself then the
specific instance of that functon is completely decided by the calling
polymorphic function. Essentially the inner function instance depends on how
the outer function is instantiated i.e. how the type parameters of the outer
function are chosen.

Such a polymorphic function that instantiates another polymorphic function
locally depending on its own instance is called a rank-2 polymorphic function.
Similarly if the inner function instantiates another polymorphic function
locally then we get a rank-3 polymorphism and so on.

The scoped quantification is introduced by the ``XRankNTypes`` GHC extension.

+----------------------+--------+--------------------------------------------------------------------------------+-------------------------+
| Polymorphic Functions| Rank3  | ``f :: (Rank2 polymorphic function type) -> b``                                | Abstract functions      |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | f :: ((forall a. a -> a) -> Int) -> Int                                        |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | Rank can be determined by counting the nesting level of the type variable      |                         |
|                      +--------+--------------------------------------------------------------------------------+                         |
|                      | Rank2  | ``f :: (Rank1 polymorphic function type) -> b``                                |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | This function itself may be monomorphic but it accepts a polymorphic function  |                         |
|                      |        | as an argument                                                                 |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | The key point is that the instantiation of the polymorphic function passed as  |                         |
|                      |        | argument is decided by this function.                                          |                         |
|                      |        +--------------------------------------------------------------------------------+                         |
|                      |        | f :: (forall a. a -> a) -> Int                                                 |                         |
|                      +--------+--------------------------------------------------------------------------------+                         |
|                      | Rank1  | ``f :: a -> b`` where type variable `a` represents values of Rank0             |                         |
+----------------------+--------+--------------------------------------------------------------------------------+-------------------------+
| Monomorphic Functions         | ``f :: Char -> Int``                                                           | Concrete function       |
|                               |                                                                                | Abstract value          |
|                               |                                                                                | Polymorphic value       |
+-------------------------------+--------------------------------------------------------------------------------+-------------------------+
| Concrete Data Values          | ``f :: Int``                                                                   | Monomorphic value       |
+-------------------------------+--------------------------------------------------------------------------------+-------------------------+

Any of the type parameters of a function can be made locally quantified by
grouping it with a forall keyword. For example::

  f :: a -> a             -- implicitly universally quantified
  f :: forall a. a -> a   -- explicitly universally quantified

  f :: (forall a. a) -> a -- the first parameter is locally quantified and is
                          -- distinct from the return type variable
  f :: a -> forall a. a   -- the return type is locally quantified and is
                          -- distinct from the first parameter.

+-----------------------------------------------------------------------------+
| .. class :: center                                                          |
|                                                                             |
|  -XRankNTypes                                                               |
+-----------------------------------------------------------------------------+
| Arbitrary-rank polymorphism                                                 |
+-----------------------------------------------------------------------------+
| Rank-1 types                                                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  f :: forall a. Ord a => a -> a                                             |
|  f :: Int -> (forall a. a -> a)                                             |
|  f :: Int -> forall a. a -> a                                               |
|  f :: Int -> Ord a => a -> a                                                |
+-----------------------------------------------------------------------------+
| Rank-2 types                                                                |
+-----------------------------------------------------------------------------+
| ``f :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int``                 |
+-----------------------------------------------------------------------------+
| Rank-3 types                                                                |
+-----------------------------------------------------------------------------+
| ``f :: ((forall a. a -> a) -> Int) -> Bool``                                |
+-----------------------------------------------------------------------------+
| Inference                                                                   |
+-----------------------------------------------------------------------------+
| For a lambda-bound or case-bound variable, x, either the programmer         |
| provides an explicit polymorphic type for x, or GHC’s type inference will   |
| assume that x’s type has no foralls in it.                                  |
+-----------------------------------------------------------------------------+

Specialization
~~~~~~~~~~~~~~

TBD

Inlining. Expansion - specialization + inlining.
