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

Curry-Howard Correspondence
---------------------------

Introduction
~~~~~~~~~~~~

+---------------------------------------+
| Type ~ Rules (Propositions) ~ Theorem |
+---------------------------------------+
| Inhabited Type ~ Proof of the theorem |
+---------------------------------------+

+------+-----------------------+------------------+
| Type | Rules                 | Inhabited values |
+------+-----------------------+------------------+
| A    | * A is a number       |                  |
|      | * A is greater than 0 |                  |
|      | * A is less than 0    | None             |
+------+-----------------------+------------------+
| B    | * B is a number       | 1, 2, 3, 4       |
|      | * B is greater than 0 |                  |
|      | * B is less than 5    |                  |
+------+-----------------------+------------------+

+-------------+---+-------------+
| Mathematics |   | Programming |
+-------------+---+-------------+
| Formulas    | ~ | Types       |
+-------------+---+-------------+
| Proofs      | ~ | Programs    |
+-------------+---+-------------+

In Haskell, a type constructor is the proof of the theorem represented by the
type.

Details
~~~~~~~

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
