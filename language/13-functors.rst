Terminology
-----------

+----------------------------+------------------------------------------------+
| Positive position          | type variable in result type of a function     |
+----------------------------+------------------------------------------------+
| Negative position          | type variable in arguments of a function       |
+----------------------------+------------------------------------------------+

* functors vs functions, args
* output vs input vs both side functors
* covariant bifunctors
* all kinds of functors

Context of Data
---------------

Any data object is in a certain context or a certain world. The world consists
of data and operations on data. The type of the data defines the world or
context of the data.

Transforming the context of data
--------------------------------

Functors are the transformation in our transformation/composition duo.

A functor is a type level transformation, transforming the type or the context
of a value. Just like a function is a value level transformation.

* Functor | Data that can be transformed | requires single object, enables transformation (change of context) of a single object

Transforms
~~~~~~~~~~

* Functions | Concrete objects |
* Functors  | Types            | Type functions, functions
* Natural transformation |

* Tensors   |

The two maps of the functor -- Haskell-types-to-Haskell-types and
Haskell-functions-Haskell-functions -- are the type constructor f and the
function fmap.

An instance of Functor is a type constructor (of kind * -> \*). An example is
Maybe.

Functors
--------

We have common basic primitives (functions) and apply them in many different
ways depending on the type i.e. the behavior is a function of the type on which
we are applying them. Intuitions:

* Functor, Applicative and Monads are ways to compose and reuse.
* A function with an argument transforms its argument
* A Functor allows us to apply a transform to that transform. That is
  we are able to use a normal function in a different way depending on
  the Functor type i.e. we are combining the peculiar behavior of a new type to an
  existing function. Pitfall - do not stereotype the concept by thinking only about a
  collection type like list.
* A functor allows reuse of a function from one context to another

Functor
-------

A Functor type is a type that acts like a function that modifies another type.
For example a type ``a`` can be modified into another type ``f a`` by the
Functor type ``f``.

A type is a functor if we can apply a function to the type
or value wrapped in it. A type has its own unique way of applying the
function. A function can be applied to the type in different ways resulting in
different types of Functors.

-- covariant Functor, the values in the Functor are consumed by fmap
-- prey functor, 'a' is to be eaten. map modifies the value in the functor.
-- Shall we call it an output functor, it modifies the output
fmap      :: (a -> b) -> f a -> f b

-- contravariant Functor, the values in the Functor will consume values
-- produced by contramap
-- predator or hungry functor, will eat 'b' and produce something that will eat
-- Shall we call it an input functor, it modifies the input
'a'. contrmap modifies what the functor eats
contramap :: (a -> b) -> f b -> f a

data WithInt a = WithInt (Int -> a)  -- covariant on 'a'
data MakeInt a = MakeInt (a -> Int)  -- contravariant on 'a'

When a type variable appears in positive position, the data type is covariant
with that variable. When the variable appears in negative position, the data
type is contravariant with that variable.

type Callback a = a -> IO ()
-- newtype CallbackRunner a = CallbackRunner (Callback a -> IO ())
-- Expands to:
newtype CallbackRunner a = CallbackRunner ((a -> IO ()) -> IO ())

In a -> IO (), a is in negative position. In (a -> IO ()) -> IO (), a -> IO ()
is in negative position. Now we just follow multiplication rules: when you
multiply two negatives, you get a positive. As a result, in (a -> IO ()) -> IO
(), a is in positive position, meaning that CallbackRunner is covariant on a,
and we can define a Functor instance.

The value that we are mapping to could be a concrete value or a function. In
case it is a concrete value the only possibility is to have a covariant
functor. If it is a function then we can have covariant, contravariant or
profunctor.

Diagram:

Covariant: v> - >f
Contravariant: f> - >v
Profunctor: f1> - >v - >f2

Examples
~~~~~~~~

List: A list type is an ordered collection of values. Applying a
function on a list is applying the function on each item in the list and
therefore producing another transformed list.

>> fmap id [1,2,3]
[1,2,3]

IO: IO type contains an action or code that will result in an input
or an output. Applying a function to IO is equivalent to applying the
function to the value resulting from executing the io code, after input
or before output.

Maybe: apply the function to the value if it exists otherwise stays
Nothing.

Utility Functors
----------------

* Identity
* Const

Profunctors
-----------

p a b -- a is in the input position and b in the output position
      -- i.e. the shape is a -> b

The value being mapped to could be concrete or a function. In case of
profunctor it has to be a function.

A function is asymmetric, it takes several inputs and produces one output.
Inputs are also called negative postion and output as positive position.

A covariant functor has a value in positive position which means the mapped
function consumes it.
A contravariant functor has a value in a negative postion which means the
mapped function produces the value by sucking in some other value.

A profunctor has two functions to map one to produce and one to consume and
therefore can be applied at both ends of the target. It is therefore more
general. It can transform a value on the way in and on the way out.

Sieve p f - (sieve :: p a b -> a -> f b) closes the input end of the profunctor
and puts the result inside a functor (f b)

Star f d c = Star { runStar :: d -> f c } where f is a functor. Star f, has a
profunctor instance, so this function inside Star can be dimapped.

Representable functors:

* functions can be represented as set values (data type)
* (-> x) is representable
* if a functor is isomorphic to (-> x) then it must be representable
* if we can provide two natural transformations from the functor to (-> x)
  functor and back then we can prove that it is representable.
* tabulate transforms a (-> x) to our functor for all argument values
* index takes a value and the representation and transforms it back to (-> x)
* tabluate . index = id

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x)

instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

In particular, it turns out that functors that are based on product types can
be represented with sum types, and that sum-type functors are not in general
representable (example: the list functor).

Finally, notice that a representable functor gives us two different
implementations of the same thing — one a function, one a data structure. They
have exactly the same content — the same values are retrieved using the same
keys.

A Profunctor p is Representable if there exists a Functor f such that p d c is
isomorphic to d -> f c.

tabulate and sieve form two halves of an isomorphism.

Strong:
This describes profunctor strength with respect to the product structure of Hask.
A strong profunctor allows the monoidal structure to pass through.

Closed:
A closed profunctor allows the closed structure to pass through.

* https://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html
* https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors

Free Functor
------------

* https://hackage.haskell.org/package/free-functors

::

  data Lan g a where
       Lan :: g x -> (x -> a) -> Lan g a

     instance Functor (Lan g) where
       fmap f (Lan gx h) = Lan gx (f . h)

     lan :: g a -> Lan g a
     lan ga = Lan ga id

Functors
--------

* Functor

  * Covariant - producers ( f a --> producer of a)

    * Applicative - combines producers

      * Alternative
  * Contravariant - consumers ( f a --> consumer of a)

    * Divisible - combines consumers

      * Decidable

+-----------------------------------------------------------------------------------+
| Product (a,b)                                                                     |
+-----------------------------------+-----------------------------------------------+
| Applicative                       | Divisible                                     |
+===================================+===============================================+
| pure :: a -> f a                  | conquer :: f a                                |
|                                   |                                               |
|                                   | conquer :: (a -> ()) -> f a -- theoretically  |
+-----------------------------------+-----------------------------------------------+
| (<*>) :: f (a -> b) -> f a -> f b | divide :: (a -> (b, c)) -> f b -> f c -> f a  |
+-----------------------------------+-----------------------------------------------+

+---------------------------------------------------------------------------------------+
| Sum (Either a b)                                                                      |
+--------------------------------+------------------------------------------------------+
| Alternative                    | Decidable                                            |
+================================+======================================================+
| ``empty :: f a``               | ``lose :: (a -> Void) -> f a``                       |
+--------------------------------+------------------------------------------------------+
| ``(<|>) :: f a -> f a -> f a`` | ``choose :: (a -> Either b c) -> f b -> f c -> f a`` |
+--------------------------------+------------------------------------------------------+

* Profunctor (pipes - producer as well as consumer)
    (p a b --> consumer of a and producer of b)

+-----------------------------------------+------------------------------------------+
| Product (a,b)                                                                      |
+-----------------------------------------+------------------------------------------+
| Strong                                  | Costrong                                 |
+-----------------------------------------+------------------------------------------+
| ``first' :: p a b -> p (a, c) (b, c)``  | ``unfirst :: p (a, d) (b, d) -> p a b``  |
+-----------------------------------------+------------------------------------------+
| ``second' :: p a b -> p (c, a) (c, b)`` | ``unsecond :: p (d, a) (d, b) -> p a b`` |
+-----------------------------------------+------------------------------------------+

+----------------------------------------------------------------------------------------------------------+
| Sum (Either a b)                                                                                         |
+----------------------------------------------------+-----------------------------------------------------+
| Choice                                             | Cochoice                                            |
+----------------------------------------------------+-----------------------------------------------------+
| ``left' :: p a b -> p (Either a c) (Either b c)``  | ``unleft :: p (Either a d) (Either b d) -> p a b``  |
+----------------------------------------------------+-----------------------------------------------------+
| ``right' :: p a b -> p (Either c a) (Either c b)`` | ``unright :: p (Either d a) (Either d b) -> p a b`` |
+----------------------------------------------------+-----------------------------------------------------+

  * Closed
    closed :: p a b -> p (x -> a) (x -> b)

  * Strong + Choice = Traversing
  * Traversing + Closed = Mapping

References
----------

* https://bartoszmilewski.com/2015/07/29/representable-functors/
