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

