Terminology
-----------

.. _Curry-Howard Correspondence: https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence

+---------------------------------+-----------------------------------------------------------------+
| Proposition                     | A statement which can be true or false e.g. A -> B; if A then B.|
+---------------------------------+-----------------------------------------------------------------+
| Conjunction                     | logical AND                                                     |
+---------------------------------+-----------------------------------------------------------------+
| Disjunction                     | logical OR                                                      |
+---------------------------------+-----------------------------------------------------------------+
| Top (T)                         | A tautology, always true                                        |
+---------------------------------+-----------------------------------------------------------------+
| Bottom (_|_ or F)               | A contradiction, always false                                   |
+---------------------------------+-----------------------------------------------------------------+
| Theorem                         | A set of propositions representing a true statement             |
+---------------------------------+-----------------------------------------------------------------+
| Type                            | Denotes certain rules that a value should conform to            |
+---------------------------------+-----------------------------------------------------------------+
| Inhabited                       | A type is inhabited if a value of that type exists              |
+---------------------------------+-----------------------------------------------------------------+
| Isomorphic                      | A is isomorphic to B when B represents A in just another form   |
|                                 | and vice versa                                                  |
+---------------------------------+-----------------------------------------------------------------+
| `Curry-Howard Correspondence`_  | If a type is inhabited it proves a theorem                      |
+---------------------------------+-----------------------------------------------------------------+
| Universal Quantification        | forall `a`: the proposition is true for all values of a         |
+---------------------------------+-----------------------------------------------------------------+
| Existential Quantification      | exists `a`: the proposition is true for some values of a        |
+---------------------------------+-----------------------------------------------------------------+
| Lambda Calculus                 | a formal system in mathematical logic for                       |
|                                 | expressing computation based on function abstraction and        |
|                                 | application using variable binding and substitution.            |
+---------------------------------+-----------------------------------------------------------------+
| Introduction rule               | When a constructor is used on the RHS to construct,             |
|                                 | it is called an `introduction rule` in type theory.             |
+---------------------------------+-----------------------------------------------------------------+
| Elimination rule                | When a constructor is used on the LHS to pattern                |
|                                 | match or destruct a data structure, it is called                |
|                                 | an `elimination rule` in type theory.                           |
+---------------------------------+-----------------------------------------------------------------+

Curry-Howard Isomorphism
------------------------

+---------------------------------------+
| Type ~ Rules (Propositions) ~ Theorem |
+---------------------------------------+
| Inhabited Type ~ Proof of the theorem |
+---------------------------------------+

+-------------------------------------------------+
| Example:                                        |
+------+-----------------------+------------------+
| Type | Rules                 | Inhabited values |
+======+=======================+==================+
| A    | * A is a number       |                  |
|      | * A is greater than 0 |                  |
|      | * A is less than 0    | None             |
+------+-----------------------+------------------+
| B    | * B is a number       | 1, 2, 3, 4       |
|      | * B is greater than 0 |                  |
|      | * B is less than 5    |                  |
+------+-----------------------+------------------+

Curry-Howard Correspondence
---------------------------

+-------------+---+-------------+
| Mathematics |   | Programming |
+=============+===+=============+
| Formulas    | ~ | Types       |
+-------------+---+-------------+
| Proofs      | ~ | Programs    |
+-------------+---+-------------+

Formulas & Types
~~~~~~~~~~~~~~~~

+----------------------------+-----------------------------------+
| Logic (Formulas)           | Programming (Types)               |
+============================+===================================+
| universal Quantification   | generalised product type (Π type) |
+----------------------------+-----------------------------------+
| existential Quantification | generalised sum type (Σ type)     |
+----------------------------+-----------------------------------+
| implication                | function type                     |
+----------------------------+-----------------------------------+
| Conjunction                | product type                      |
+----------------------------+-----------------------------------+
| Disjunction                | sum type                          |
+----------------------------+-----------------------------------+
| true formula               | unit type                         |
+----------------------------+-----------------------------------+
| false formula              | bottom type                       |
+----------------------------+-----------------------------------+

Proofs & Programs
~~~~~~~~~~~~~~~~~

+------------------------------+-----------------------------+
| Natural Deduction (Proofs)   | Lambda Calculus (Programs)  |
+==============================+=============================+
| axiom                        | variable                    |
+------------------------------+-----------------------------+
| introduction rule            | constructor (Abstraction)   |
+------------------------------+-----------------------------+
| elimination rule             | destructor (Apply)          |
+------------------------------+-----------------------------+
| normal deduction             | normal form                 |
+------------------------------+-----------------------------+
| normalisation of deductions  | weak normalisation          |
+------------------------------+-----------------------------+
| provability                  | type inhabitation problem   |
+------------------------------+-----------------------------+
| intuitionistic tautology     | inhabited type              |
+------------------------------+-----------------------------+

Haskell Type system logical connectives
---------------------------------------

Note, there is nothing precise in it , this is just to get a rough intuition
about the type system. The Haskell type-system provides the following logical
connectives that make a functionally complete system:
  Composition:
    Sum = OR
    Product = AND
  Transformation
    function application = ->
  Bottom = _|_

Note that the Haskell sum type is a tagged union and not really a coproduct of
independent types unless we are using GADTs.

We can recover NOT from implication and bottom: NOT = A -> Bottom.
We need only one of AND and OR and both of the rest to make a functionally
complete system i.e. {NOT, AND} or {NOT, OR}.

When we define a data type `A` then we know that type `A` exists i.e. there is
a way to construct it.  If we have a function signature or inferred call A -> B
-> C -> D, it means A AND B AND C -> D. Which says if types a, b and c exist
then type d is valid. All such logic equations implied by the types used
eveywhere in the program taken together must be satisfiable for the program to
be accepted by the compiler.  Otherwise the program gets rejected meaning the
types are inconsistent.

References
----------

* http://www.haskellforall.com/2017/02/the-curry-howard-correspondence-between.html
* https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism
* https://en.wikipedia.org/wiki/Duality_(mathematics)
* https://en.wikipedia.org/wiki/Functional_completeness
* https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
* https://en.wikipedia.org/wiki/Combinatory_logic
* https://stackoverflow.com/questions/44034591/is-haskells-type-system-isomorphic-to-an-inconsistent-logic-system-if-so-what
* https://en.wikipedia.org/wiki/SKI_combinator_calculus
