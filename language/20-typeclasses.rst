Type Classes
============

Terminology
-----------

* Dictionary
* Superclass

A Bare Type
-----------

Any type creates an indirection or a context in which the value is to be
interpreted. It says you cannot directly operate on me, you will have to use my
way of doing things.  A value wrapped in a type cannot be operated directly or
accidentally. No existing function will operate on a newly born type.  We have
to define type specific operations for it.

A bare type is pure data definition isolated from the world. We need to connect
it to the world by defining operations on it.

Adding it to typeclasses
~~~~~~~~~~~~~~~~~~~~~~~~

Standard operations are bundled in typeclasses.  Typeclasses are also a way of
advertising (via constraints) that this type supports these operations and you
can use those operations. So the new type can fit into a lot of existing
infrastructure automatically - existing hihger level typeclasses can use it,
existing operations can use it.  Some typeclass operations can be automatically
defined using deriving.

Defining Operations
~~~~~~~~~~~~~~~~~~~

We can also define operations on a type without making it a member of
typeclasses.


TBD - picture of a bare type, then with operations, typeclass oeprations

Deriving
--------

GeneralizedNewTypeDeriving - how do you know what all instances can be derived
automatically by GHC?

Typeclass Definition
--------------------

+-----------------------------------------------------------------------------+
| Typeclass definition                                                        |
+=============================================================================+
| ::                                                                          |
|                                                                             |
|  class Default a where                                                      |
|   def :: a                                                                  |
+-----------------------------------------------------------------------------+
| The type variable `a` represents any member type of the type class.         |
+-----------------------------------------------------------------------------+
| `def` is like any other function, the type signature of `def` is            |
| polymorphic and determined by the typeclass. The effective signature of     |
| `def` is equivalent to:                                                     |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   def :: Default a => a                                                     |
+-----------------------------------------------------------------------------+

Instances
---------

Unlike functions with polymorphic parameters, which provide a single definition
for a whole set of types (known as `parametric polymorphism`), typeclasses
allow the function definition to be dependent on the type.  Each type can have
its own definition of the polymorphic functions defined by a type class, this
is called `ad-hoc polymorphism`.

An instance of a typeclass provides the definition of the typeclass functions
corresponding to a particular type.

+-----------------------------------------------------------------------------+
| Typeclass instance                                                          |
+=============================================================================+
| ::                                                                          |
|                                                                             |
|  instance Default Int where                                                 |
|   def = 0                                                                   |
|                                                                             |
|  instance Default String where                                              |
|   def = ""                                                                  |
+-----------------------------------------------------------------------------+

Resolving Instances
-------------------

When the function `def` is called, its definition is provided by one of the
instances of `Default`. The definition is uniquely determined by the actual
signature of the function inferred at the call site::

  def :: Default a => a

  def :: Int -- 0
  def :: String -- ""
  def :: Char -- Doesn't compile, no instance for "Default Char"

+-----------------------------------------------------------------------------+
| Ambiguity check:                                                            |
| To determine the typeclass instance, we must be able to fully resolve the   |
| signature of `def` at the call site. This will result in an error:          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class Default a where                                                      |
|   def :: Int                                                                |
+-----------------------------------------------------------------------------+
| Here, type `a` and therefore the specific instance of the class, cannot be  |
| determined by a call to `def` e.g.                                          |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  def :: Default a => Int                                                    |
|                                                                             |
|  def :: Int -- 'a' cannot be determined here so compiler cannot determine   |
|             -- which instance to use                                        |
+-----------------------------------------------------------------------------+

Typeclass Definition - Again
----------------------------

+-----------------------------------------------------------------------------+
| Put a constraint to restrict the types that can be a member of the class    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class Num a => Default a where                                             |
|   def :: a                                                                  |
+-----------------------------------------------------------------------------+
| The signature of `def` is now equivalent to:                                |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   def :: (Num a, Default a) => a                                            |
+-----------------------------------------------------------------------------+
| A typeclass can provide a default implementation of a function              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class Num a => Default a where                                             |
|   def :: a                                                                  |
|   def = 0 -- default implementation                                         |
|                                                                             |
|  instance Default Int  -- Will use the default implementation of def        |
+-----------------------------------------------------------------------------+
| One class function can be defined in terms of another class function        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  class Num a => Default a where                                             |
|   def1 :: a                                                                 |
|   def2 :: a                                                                 |
|   def1 = def2 - 1                                                           |
|   def2 = def1 + 1                                                           |
+-----------------------------------------------------------------------------+
| If we define def1 then def2 will have a default implementation and vice     |
| versa.                                                                      |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  instance Default Int where                                                 |
|   def1 = 0         -- def2 is automatically defined to 1                    |
+-----------------------------------------------------------------------------+

-XConstrainedClassMethods
~~~~~~~~~~~~~~~~~~~~~~~~~

Allow class methods to constrain class type variables::

  class Seq s a where
    fromList :: [a] -> s a
    elem     :: Eq a => a -> s a -> Bool

Typeclass Instances - Again
---------------------------

default instance
~~~~~~~~~~~~~~~~

Wildcard instance that applies when a specific instance does not::

  instance C a where
    op = ... -- Default

-XTypeSynonymInstances
~~~~~~~~~~~~~~~~~~~~~~
::

  type Point a = (a,a)
  instance C (Point a)   where ...

-XFlexibleInstances
~~~~~~~~~~~~~~~~~~~
::

  instance C (Maybe Int) where ...   -- allows arbitrary nested types

-XFlexibleContexts
~~~~~~~~~~~~~~~~~~
::

  instance (C t1 ... tn) => ...

Type Family Application in Instance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Is illegal. But there is a workaround for a single instance::

  type family Fam t
  instance (Fam Int ~ famint) => C famint

* https://ghc.haskell.org/trac/ghc/ticket/3485

Constraints are a good way to restrict the polymorphism. In fact we can even
make a class which represents a single type.

::

  type Module = R ("a" := Int, "b" := String)

  class (a ~ Module) => Default a where
      def :: ("b" := String) -> a

  instance (x ~ R ("a" := Int, "b" := String)) => Default x  where
      def t = R (#a := 0 :: "a" := Int) :*: R t

Multi-parameter Typeclasses
---------------------------

Functional Dependencies
~~~~~~~~~~~~~~~~~~~~~~~

::

  class MonadBase b m => MonadBaseControl b m | m -> b where

We can read ``m -> b`` as ``m determines b``.  The part after ``|`` is a
functional dependency which says ``m`` uniquely determines ``b`` i.e. for the
same ``m`` there cannot be more than one ``b``. In other words, ``b`` is a
function of ``m`` i.e.  ``f m = b`` for some f.

-XAllowAmbiguousTypes can be useful with functional dependencies.

Infix Constructor syntax
~~~~~~~~~~~~~~~~~~~~~~~~

::

  class a :=: b where ...


Multi-parameter Typeclass Instances
-----------------------------------

Instance declarations
---------------------

::

  instance <context> => <head> where ...
  instance (assertion1, ..., assertionn) => class type1 ... typem where ...

  instance <context> => C (T a1 ... an) : Haskell98
  instance <context> => C (T1 a1 ... an) (T2 b1 ... bn) : Multiparameter

Examples:

Overlapping & Incoherent Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Try `Int Bool` or `Int [Int]` in the folowing::

  instance {-# OVERLAPPABLE #-} context1 => C Int b     where ...  -- (A)
  instance {-# OVERLAPPABLE #-} context2 => C a   Bool  where ...  -- (B)
  instance {-# OVERLAPPABLE #-} context3 => C a   [b]   where ...  -- (C)
  instance {-# OVERLAPPING  #-} context4 => C Int [Int] where ...  -- (D)

* More specific instance is chosen when possible
* When ambiguous, errors out unless ``-XIncoherentInstances`` is used

UndecidableInstances
~~~~~~~~~~~~~~~~~~~~

Instance termination rules
^^^^^^^^^^^^^^^^^^^^^^^^^^

Paterson:
  - occurrence of `t` in constraint <= occurrence of `t` in head
  - length of constraint < length of head
  - no type functions allowed in constraint

These are not okay::

    -- Context assertion no smaller than head
    instance C a => C a where ...
    -- (C b b) has more occurrences of b than the head
    instance C b b => Foo [b] where ...

Type variables present in the context but not in the head `may` cause
typechecker loop::

  class D a
  class F a b | a->b
  instance F [a] [[a]]
  instance (D c, F a c) => D [a]   -- 'c' is not mentioned in the head

Coverage:
  For each functional dependency, ⟨tvs⟩left -> ⟨tvs⟩right, of the class, every
  type variable in S(⟨tvs⟩right) must appear in S(⟨tvs⟩left), where S is the
  substitution mapping each type variable in the class declaration to the
  corresponding type in the instance head.

-XUndecidableInstances
^^^^^^^^^^^^^^^^^^^^^^

Allows class synonym::

  class (C1 a, C2 a, C3 a) => C a where { }
  instance (C1 a, C2 a, C3 a) => C a where { }
  f :: C a => ...

Relaxes the paterson conditions described above.

Deriving Instances
------------------

* You can’t use deriving to define instances of a data type with existentially
  quantified data constructors.

+------------------------------+----------------------------------------------+
| -XDeriveFunctor              | deriving Functor                             |
+------------------------------+----------------------------------------------+
| -XDeriveFoldable             | deriving Foldable                            |
+------------------------------+----------------------------------------------+
| -XDeriveTraversable          | deriving Traversable                         |
+------------------------------+----------------------------------------------+
| -XDeriveDataTypeable         | deriving (Typeable, Data)                    |
+------------------------------+----------------------------------------------+
| -XGeneralizedNewtypeDeriving | Everything that the underlying type supports?|
+------------------------------+----------------------------------------------+

* http://cs.brynmawr.edu/~rae/talks/2013/hiw-roles.pdf GeneralizedNewtypeDeriving is now type-safe

Associated Types
----------------

When the types in the type signatures of class functions cannot be mapped to
the member type directly we can use type functions to map them to the desired
types.  Such type functions are called associated types.

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

Examples: Variable argument function
------------------------------------

We can overload a function based on its signature.

class Signature a where
  func :: a

instance Signature (String -> String)
  where func s1 = s1

instance Signature (String -> String -> String)
  where func s1 s2 = s1 ++ s2

Problem
-------


class AddMod a where
    addmod :: a

addmod :: x -> z
addmod :: x -> y -> z

How do we fix return type z in the signature in the class and represent the
arguments using a variable? Is it even possible?

Can we create a constraint to specify that the return type of a function is
fixed but arguments can be anything, any number?

* http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn
* http://chris-taylor.github.io/blog/2013/03/01/how-haskell-printf-works/

References
----------

* https://wiki.haskell.org/Typeclassopedia
* https://ocharles.org.uk/blog/guest-posts/2014-12-15-deriving.html
* http://stackoverflow.com/questions/8546335/ambiguous-type-variable-a0-in-the-constraints
* https://stackoverflow.com/questions/12645254/ghc-code-generation-for-type-class-function-calls
