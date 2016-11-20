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
| Currying                   | the generating of intermediate functions on the way toward a final result|
+----------------------------+------------------------------------------------+
| Partial function           |  A function that is not defined for some values
|                            | of its arguments |
+----------------------------+------------------------------------------------+
|                            |                                                |
+----------------------------+------------------------------------------------+

* Universal quantification

Function
--------

signature
f :: a -> b -> c -> d

graphical rep:

a  b  c  d
\/ \/ \/ >
+-------+
| Stuff |
+-------+

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

Type Space
----------

Type functions are first class objects in type space like value functions are
first class objects in value space. Which means type functions can be passed
around like normal types.

Value Space
-----------
Representing Values and their types
===================================

A value is represented by a triangle. The type of the value is represented by a
sequence and color of dots on the sides of the traingle.

A function is represented by a triangle with as many triangular slots in the
base as the number of inputs it takes. The slot is lined with a sequence of
dots representing the type of the input. The dots on the triangle of a value
must match with the dots in the triangular slot. The dots can also be small
triangles themselves.

::

 |\
 | \
 |  \
 |  /
 | /
 |/

Each dot could be a pit or a rise and it can have 3 colors (RGB). So it can
represent 6 states. If we have 3 (side) + 3 (side) + 1 (tip) dots on each
triangle we can represent 6^7 = 279936 types.

As soon as the slots in the triangle are filled it becomes a concrete value
i.e. perfect triangle in shape. Or if it returns a function then it becomes a
slotted triangle again.

If it returns a function then it will have more slots at the lower side which
can be filled after the other input slots are filled. In fact it is actually a
function with more inputs. e.g.

f :: a -> b -> (c -> d) = f :: a -> b -> c -> d


Syntax
------

* partial type signatures (_ wildcard)
