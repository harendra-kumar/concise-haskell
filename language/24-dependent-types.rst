Dependent Type Programming
--------------------------

A natural number datatype::

  data Nat where
    Zero :: Nat
    Succ :: Nat → Nat

can automatically be used at the type level to indicate the
length of a vector represented by a GADT.

::

  data Vec :: ∗ → Nat → ∗ where
    VNil ::Vec a ’Zero
    VCons :: a → Vec a n → Vec a (’Succ n)

(The single quotes in front of the data constructor names indicate
that they were promoted from the expression language.)

Furthermore, type families can express functions on this typelevel data, such
as one that computes whether one natural number is less than another.::

  type family (m ::Nat) :<(n ::Nat) ::Bool
  type instance m :< ’Zero = ’False
  type instance ’Zero :<(’Succ n) = ’True
  type instance (’Succ m):<(’Succ n) = m :<n

However, there is still at least one significant difference between programming
in Haskell and in full-spectrum dependently typed languages. Haskell enforces a
phase separation between runtime values and compile-time types. Consequently,
to express the dependency between the value of one runtime argument and the
compiletime type of another requires the definition and use of singleton
types—types with only one non-⊥ value

For example, consider the safe indexing operation for vectors
below, called nth. This operation ensures that the index m is less
than the length of the vector n with the constraint (m:<n) ∼ True.
However, because nth requires the index at runtime, this index
cannot only be a type. We must also include a runtime witness for
this index, called a singleton, that can be used for computation.
The type of singleton values for natural numbers is SNat, a GADT
indexed by a type of kind Nat.::

  data SNat ::Nat → ∗ where
  SZero ::SNat ’Zero
  SSucc :: ∀ (n ::Nat). SNat n → SNat (’Succ n)

A graphical schematic of the relationship between the original
datatype, the promoted kind, and the derived singleton type can be
seen in Figure 1. Because the constructors of SNat mirror those of
the kind Nat, only one non-⊥ term exists in each fully-applied type
in the SNat family. Hence, these types are called singleton types. In
such types, the type variable indexing the type and the one non-⊥
term of that type are always isomorphic. Thus, singleton types can
be used to force term-level computation and type-level computation
to proceed in lock-step.

This singleton is the first runtime argument of the nth function
and determines the element of the vector that should be returned.::

  nth ::(m :<n) ∼ ’True ⇒ SNat m → Vec a n → a
  nth SZero (VCons a ) = a
  nth (SSucc sm’) (VCons as) = nth sm’ as

The nth code type checks in the second case because pattern
matching refines the type variables m and n to be headed by Succ.
Therefore, the constraint m :< n reduces to the simpler constraint
required by the recursive call to nth. Furthermore, GHC observes
that indexing must succeed. An attempt to add the following case
to the pattern match results in a compile-time error.::

  nth m VNil = error "index out of bounds"

References
----------

* http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf Dependently Typed Programming with Singletons
