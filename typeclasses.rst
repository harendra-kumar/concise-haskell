Type Classes
============

* Specifies functions to be implemented by member type or type group
  (multiparameter)
* Constraints can be applied to restrict the member types to certain type
  classes (called superclasses)
* Functions could be defined in terms of each other so that only one needs to
  implemented.

Terminology
-----------

* Dictionary
* Superclass

Class Definition
----------------

class ClassName t where
  ...

t is a type variable which represents a member of the class.

Infix Constructor syntax
~~~~~~~~~~~~~~~~~~~~~~~~

::

  class a :=: b where ...


Instance declarations
---------------------

::

  instance <context> => <head> where ...
  instance (assertion1, ..., assertionn) => class type1 ... typem where ...

  instance <context> => C (T a1 ... an) : Haskell98
  instance <context> => C (T1 a1 ... an) (T2 b1 ... bn) : Multiparameter

Examples::

default instance
~~~~~~~~~~~~~~~~

Wildcard instance that applies when a specific instance does not::

  instance C a where
    op = ... -- Default

-XConstrainedClassMethods
~~~~~~~~~~~~~~~~~~~~~~~~~

Allow class methods to constrain class type variables::

  class Seq s a where
    fromList :: [a] -> s a
    elem     :: Eq a => a -> s a -> Bool

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
~~~~~~~~~~~~~~~~~~

* You can’t use deriving to define instances of a data type with existentially
  quantified data constructors.

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

References
----------

https://wiki.haskell.org/Typeclassopedia
