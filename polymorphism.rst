Polymorphism
============

Terminology
-----------

+-------------------------+---------------------------------------------------+
| Polymorphism            | A value or function can have multiple types       |
+-------------------------+---------------------------------------------------+
| Parametric Polymorphism | The type of a value is defined in terms of a type |
|                         | parameter or variable.                            |
+-------------------------+---------------------------------------------------+
| Ad-hoc Polymorphism     | A value is given different definitions depending  |
|                         | on the type                                       |
+-------------------------+---------------------------------------------------+
| Bounded Polymorphism    | Another name for ad-hoc polymorphism, bounded     |
|                         | because it is limited by the instances or         |
|                         | definitions.                                      |
+-------------------------+---------------------------------------------------+
| Parametric type         | A data, type synonym, data family or type family  |
|                         | parameterized by a type variable.                 |
+-------------------------+---------------------------------------------------+
| Polymorphic type        | Parametrically polymorphic type with a single     |
|                         | representation of constructors based on the       |
|                         | paremeter.                                        |
+-------------------------+---------------------------------------------------+
| Data families           | Parametric data types that can be assigned        |
|                         | specialized constructor representations based on  |
|                         | the type parameters they are instantiated with    |
+-------------------------+---------------------------------------------------+
| Type synonym families   | Parametric type synonyms that can be assigned     |
|                         | specialized representations based on the type     |
|                         | parameters they are instantiated with             |
+-------------------------+---------------------------------------------------+
| Type families           | Data families and type synonym families together  |
|                         | can be referred to as type families.              |
+-------------------------+---------------------------------------------------+

Polymorphism - Expressive Power
-------------------------------

* Typeclasses: attach functions to types, the function can be different for different types.
* Polymorphic functions: Same function can work on many types
* Higher order functions

* Reuse -> conciseness -  expressive power - polymorphism is about reuse - you can abstract out the common parts in processing multiple related but slightly different types
* Composition -> reuse
* typeclass hierarchy

Polymorphic Entities
--------------------

+-------------------+-----------------------------+
| Polymorphic types | Polymorphic functions       |
+-------------------+-----------------------------+

+-------------------------------------------------------------------+
| Polymorphism ~ Abstraction                                        |
+===================================================================+
| Common part                                                       |
+-------------------------------------------------------------------+
| Abstract or variable part representing multiple concrete entities |
+-------------------------------------------------------------------+

Polymorphism in Haskell
-----------------------

+-------------------------------------------------+
| Kind Polymorphism                               |
+-------------------------------------------------+

Examples:

+-----------------------------------------------------------------------------+
| Data Type Polymorphism                                                      |
+------------+-----------------------+----------------------------------------+
| Parametric | Polymorphic types     | Generic constructor instances for      |
|            |                       | the whole family                       |
+------------+-----------------------+----------------------------------------+
| Ad-hoc     | Data families         | Specialized constructor or type        |
|            +-----------------------+ instances for each member type of the  |
|            | Type synonym families | family                                 |
+------------+-----------------------+----------------------------------------+

Examples
~~~~~~~~

+-------------------------------------+---------------------------------------+
| Polymorphic types                   | Polymorphic Functions                 |
+-------------------------------------+---------------------------------------+
| a :: *                              | id :: a -> a                          |
+-------------------------------------+---------------------------------------+
| List a :: *                         | (++) :: List a -> List a -> List a    |
+-------------------------------------+---------------------------------------+

+-----------------------------------------------------------------------------+
| Function Polymorphism                                                       |
+------------+-------------+--------------------------------------------------+
| Parametric | Polymorphic | A single generic function instance with          |
|            | Functions   | parametrically polymorphic values as arguments.  |
+------------+-------------+--------------------------------------------------+
| Ad-hoc     | Typeclasses | Specialized function instances associated with   |
|            |             | each type which is a member of the typeclass.    |
+------------+-------------+--------------------------------------------------+

Polymorphic functions operate on polymorphic data. A polymorphic function has
at least one type variable in its type. Intuitively they operate on the common
part of the objects passed to it or can use polymorphic operations on the
abstract part. They can operate on the abstract type if the type is known to
belong to a typeclass via a typeclass constraint.

+-----------------------------------------------------------------------------+
| Polymorphic Functions                                                       |
+--------+--------------------------------------------------------------------+
| Rank   | Parametric type argument variables (values or functions)           |
+========+====================================================================+
| Rank-1 | monomorphic                                                        |
+--------+--------------------------------------------------------------------+
| Rank-2 | rank-1 polymorphic or lower                                        |
+--------+--------------------------------------------------------------------+
| Rank-n | rank n-1 polymorphic or lower                                      |
+--------+--------------------------------------------------------------------+

In contrast to a parametrically polymorphic data type, each member of a type
family is deconstructed differently therefore a generic polymorphic function
cannot be used on a type family.  So type families are naturally associated
with typeclasses where we write a type instance as well as a function instance
to handle that type instance.

Examples:

+-----------------------------------------------------------------------------+
| Typeclasses                                                                 |
+------------------+----------------------------------------------------------+
| Using parametric | Typeclass functions work on types which are              |
| types            | parameterized by the typeclass member                    |
|                  | type(s).                                                 |
+------------------+----------------------------------------------------------+
| Using associated | Typeclass instance creates an instance of a type family  |
| type family      | associated with the type class. Typeclass functions work |
|                  | on this particular instance of the data type.            |
+------------------+                                                          |
| Using associated |                                                          |
| data family      |                                                          |
+------------------+----------------------------------------------------------+

TBD: picture depicting typeclass member type -> polymorphic function
TBD: picture depicting typeclass member type -> associated family ->
polymorphic function

The data types that the typeclass functions work on can of course be
polymorphic (parametric or type family). Therefore these functions are
potentially polymorphic functions.

Polymorphic functions vs Typeclass
----------------------------------

If the logic for all types can be abstracted in such a way that a single
function body parameterized by the type can be used to represent the logic then
you can use a polymorphic function. Usually logic that works on a single
parameterized type.

When different types or types of different shapes need to support an abstract
function then we have to create a typeclass with the common functions and make
each type a member of that type class. The types may be pretty much unrelated
types with different shapes.

When the types that need to support the same abstract functions are related via
a type family then we can associate the type family with the typeclass and
instantiate the family members in each typeclass instance.

Examples:
