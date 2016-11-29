* https://en.wikibooks.org/wiki/Haskell/Higher-order_functions

Terminology
-----------

+----------------------------+------------------------------------------------+
| Arity                      |                                                |
+----------------------------+------------------------------------------------+
| Application                | Function application                           |
+----------------------------+------------------------------------------------+
| Positive position          | type variable in result of the function        |
+----------------------------+------------------------------------------------+
| Negative position          | type variable in arguments of the function     |
+----------------------------+------------------------------------------------+
| Higher order function      | A function that takes a function or functions  |
|                            | as an argument                                 |
+----------------------------+------------------------------------------------+
| Currying                   | the generating of intermediate functions on the|
|                            | way toward a final result                      |
|                            | One at a time consumption of arguments         |
+----------------------------+------------------------------------------------+
| Partial function           |  A function that is not defined for some values|
|                            | of its arguments                               |
+----------------------------+------------------------------------------------+
|                            |                                                |
+----------------------------+------------------------------------------------+

* Universal quantification

Function
--------

graphical rep:

a  b  c  d
\/ \/ \/ >
+-------+
| Stuff |
+-------+

signature
f :: a -> b -> c -> d

Function type variables are implicitly universally quantified.

Functions are first class values which can be used as arguments to other
functions and can be returned by functions.

Apples, oranges, bananas as input, layered salad as output for absolute
beginners.

Function Application (currying)
===============================

A fully applied function represents a value in value space or a concrete type
in type space.

* Partial Application
* Saturated application
* Oversaturated application

f :: a -> (b -> c -> d)
f :: a -> b -> (c -> d)

Show this graphically as a hopper with 3 opening on top. You can put a fitting
cap to close the first mouth and then the second one gets activated and so on.
As soon as you close the last mouth the return value gets activated.

-- \/ \/ >
-- -- \/ >
-- -- -- > o
-- -- -- > \/ >

    +-------+
--->|       |
 -->|       | >
  ->| Stuff |
    +-------+

Each mouth has a signature and only a particular type of thing can go in it.
Sorry I don't eat oranges, only apples please!

The return value itself can have mouths and can take more values or it can be a
simple value with no mouth.

Rank 1 Polymorphism
RankNPolymorphism

The Function Application Type
-----------------------------

(->) represents a function application. A function application maps values from
one type to values of another type.  Thus (->) is a type level function which
takes two types as arguments and returns a type as output.

Kind signature:
(->) :: Type -> Type -> Type

+-----------------------------------------------------------------------------+
| (->) is just like any other type functions except that it is usually used   |
| as infix. These two are equivalent.                                         |
+----------------------+------------------------------------------------------+
| square :: Int -> Int | square :: (->) Int Int                               |
+----------------------+------------------------------------------------------+
| Int -> Int -> Int    | (->) Int ((->) Int Int)                              |
+----------------------+------------------------------------------------------+

(->) associates from right to left and not vice-versa.
thing -> (thing -> thing)

Syntax
------

* partial type signatures (_ wildcard)
