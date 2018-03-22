Functors: Functional Containers
===============================

"All problems in computer science can be solved by another level of
indirection" - David J. Wheeler

.. contents:: Table of Contents
   :depth: 1

.. sectnum::

Terminology
-----------

+------------------------+----------------------------------------------------+
| Functor                |                                                    |
+------------------------+----------------------------------------------------+
| Natural transformation |                                                    |
+------------------------+----------------------------------------------------+
| Tensor                 |                                                    |
+------------------------+----------------------------------------------------+
+-------------------+---------------------------------------------------------+
| Pure              | A pure value or pure type is used to refer to types     |
|                   | which are not wrapped in a monad type.                  |
+-------------------+---------------------------------------------------------+


Overview
--------

.. Containers and Generators, transparent or opaque boxes.
.. You can inject (and extract)stuff into a container and you can extract stuff
   from a generator.
.. A container is a traversable container, a generator is a representable
   functor.
.. A generator is a Constructor + function (representable functor) an opaque
   Moore machine.

Functors are `functional containers`. Functors can hold one or more data
elements of a certain type and in a certain structure determined by the type of
the functor.  They can be parameterized by one or more types that determines
the type of items that the functor holds.

Haskell ADTs provide a way to construct a type specific structure via
constructors and de-structure it via pattern matches.  A polymorphic type is a
type function that defines a data structure in terms of the parameter type. The
only thing that you can do with these ADTs is to construct them or deconstruct
them, they just act as a means of structured storage and retrieval of data.

A functor is a polymorphic type having at least one type parameter.
Functors provide an additional feature that makes them a very powerful
abstraction; all the `pure` Haskell functions can be `lifted` via `fmap` to
work on the functor's contents in a functor type specific way.  A functor
is a member of the `Functor` type class that implements the fmap function and
providing a type specific way to map any Haskell function to the functor's
contents.

.. An instance of Functor is a type constructor (of kind * -> \*). An example is
   Maybe. Picture or table: type => type functions => functors.

A functor defines a new container whose structure can be parameterized by any
Haskell type(s) and all Haskell functions can be lifted to work on it.  A
functor just introduces an abstract structure, a scaffolding and the rules to
work upon that structure; the types contained in the functor are opaque to the
functor it does not inspect them (corecursive types?). It introduces a type
abstraction layer in terms of the parameter types.  A functor can be
parameterized by more than one type. The type parameters are the "ports" that
allow us to adapt the structure and behavior container.  We can create
interesting structures depending on where and how in the functor data structure
they fit.

.. Notice that functors are a type level transformation or abstraction. Just
   like functions are a value level abstraction.

In categorical terms, a functor creates a mapping from Hask to Hask. The type
function maps the Hask types and the type class function `fmap` maps the Hask
morphisms or functions.

A functor can isolate its contents from the rest of the world by regulating the
construction and elimination interfaces of the type. Allowing the separation of
effectful (e.g. IO) code from pure code. Applicative and Monadic functors allow
composing effectful actions.

.. Picture: Pure world as the default box. IO Functor, List Functor etc. as
  separate boxes.

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

Types of Functors
-----------------

As discussed earlier there are two fundamental types of values in Haskell,
functions and non-functions. A functor may contain either of those. Functors
can be classified based on the number of type parameters and on which side of
the pipe the parameters sit i.e. input or output side of the type definition.
If the same parameter appears on both sides the type cannot be a functor.

One ended functors
~~~~~~~~~~~~~~~~~~

::

  our type is at the one end of a pipe.
  covariant or contravariant
  Values are values in Identity functor.
  Functions are applicatives using Identity functor?
  Continuations are monads using Identity functor?
  More types are instances of one-ended functors because it requires little
  structural constraints on the type.
  A monad can return any type
  a comonad can accept any type.

Covariant vs Contravariant
~~~~~~~~~~~~~~~~~~~~~~~~~~

A covariant functor has the type parameter in a positive position of its
definition which means it is an output and therefore fmap has the ability to
influence the output of the data structure.

A contravariant functor has the type parameter in a negative position or an
input position of its definition which means fmap has the ability to influence
the input of the data structure.

* Covariant - producers ( f a --> produces a)
* Contravariant - consumers ( f a --> consumes a)

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

Double ended functors (Profunctors)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have two types at two different ends of a pipe both can be mapped.
Since this requires more structure, lesser number of types can conform to
this structure. But it provides more powerful ways to combine those types.
If we map both ends to id then the value just remains as the original pipe.
They can be composed with effectful composition, if we use an "Identity
profunctor" the composition will be just like function composition.
A profunctor can accept any type on both ends.

+---------------+--------+-------+------------+--------+--------+------------+
| Type Class    | Type   | input | imap       | output | omap   | Examples   |
+===============+========+=======+============+========+========+============+
| Functor       | f a    |       |            | a      | fmap   | Identity   |
| (output)      |        |       |            |        |        |            |
+---------------+--------+-------+------------+--------+--------+------------+
| Contravariant | c a    | a     | contramap  |        |        | Const a    |
| (input)       |        |       |            |        |        +------------+
|               |        |       |            |        |        | Comparison |
|               |        |       |            |        |        +------------+
|               |        |       |            |        |        | Predicate  |
+---------------+--------+-------+------------+--------+--------+------------+
| Bifunctor     | f a b  |       |            | a, b   | bimap  | Const      |
| (output)      |        |       |            |        +--------+------------+
|               |        |       |            |        | first  | (,)        |
|               |        |       |            |        +--------+------------+
|               |        |       |            |        | second | Either     |
+---------------+--------+-------+------------+--------+--------+------------+
| Profunctor    | p a b  | a     |            | b      | dimap  | a -> b     |
| (input,       |        |       |            |        +--------+------------+
| output)       |        |       |            |        | lmap   |            |
|               |        |       |            |        +--------+------------+
|               |        |       |            |        | rmap   |            |
+---------------+--------+-------+------------+--------+--------+------------+

.. Till this point we can merge with "Polymorphic data and Functors", the
   rest can go to "Composing with Functors"

BiFunctor Specializations
~~~~~~~~~~~~~~~~~~~~~~~~~

* Data.Bifoldable
* Data.Bitraversable

Generic Functors
----------------

Identity Functor:

+----------------+
| Identity       |
+----------------+

+-------------+
| Const       |
+-------------+

Free and Forgetful Functors
---------------------------

.. Free and forgetful are loose or informal definitions
.. Traversable containers are necessarily free and representable functors are
   potentially forgetful (functions may not be invertible). The same container
   can be free as well as forgetful.  Free are necessarily finite and forgetful
   may be infinite. Free corresponds to data and forgetful to codata?

Let's examine the two additional primitives needed for general cross functor
transformation support. A Free functor guarantees injection of values whereas a
forgetful functor guarantees extraction.

+-----------+---------------------+-------------+------------+
| Operation | Description         | Free        | Forgetful  |
+-----------+---------------------+-------------+------------+
| a -> f a  | Injection (input)   | Guaranteed  | Maybe      |
+-----------+---------------------+-------------+------------+
| f a -> a  | Extraction (output) | Maybe       | Guaranteed |
+-----------+---------------------+-------------+------------+

In general, a functor may have zero or more elements of type `a` in it.
It may or may not allow injection or extraction of a single data element
`a` to or from it.  Some types always allow injection whereas some other types
always allow extraction.  When we are  able to inject a single data element
into a functor, then it is the only element in the functor and there are no
others, we call it `free` injection. When we extract a single element from a
functor we `forget` all other elements if there are any, we call it `forgetful`
extraction.

+-----------------------------------------------------------------------------+
| Injection or extraction is not possible in a phantom type because it is not |
| inhabited at all.                                                           |
+---------------------------------------+-------------------------------------+
| data Phantom a                        | Injection: No, Extraction: No       |
+---------------------------------------+-------------------------------------+
| A function or type transformation is a one way functor, we inject one       |
| type into it but extract another, the only of its kind.                     |
+---------------------------------------+-------------------------------------+
| (->) a                                | Injection: a, Extraction: b         |
+---------------------------------------+-------------------------------------+
| Injection and (forgetful) extraction are always possible in non-recursive   |
| product types.                                                              |
+---------------------------------------+-------------------------------------+
| (e, )                                 | Injection: (e, a) Extraction: a     |
+---------------------------------------+-------------------------------------+
| In recursive product types injection may not be possible but extraction is  |
| guaranteed. However, we can construct the full data type.                   |
+---------------------------------------+-------------------------------------+
| data Cofree f a = a :< f (Cofree f a) | Injection: No, Extraction: a        |
+---------------------------------------+-------------------------------------+
| In sum types, injection is guaranteed but extraction may not be possible.   |
| However, we can use a default value (when the type allows it e.g. mempty    |
| for a Monoid) for extraction to succeed.                                    |
+---------------------------+-------------------------------------------------+
| Maybe                     | Injection: Just a, Extraction: No, when Nothing |
+---------------------------+-------------------------------------------------+
| Either l                  | Injection: Right a, Extraction: No, when Left   |
+---------------------------+-------------------------------------------------+
| []                        | Injection: [a], Extraction: No, when []         |
+---------------------------+-------------------------------------------------+

Natural Transformations
-----------------------

A function or transformation that works across functors (`f a -> g b`) is
called a natural transformation. It is a generalization of a pure
transformation (`a -> b`). Commonly one side of the transformation is pure, we
can also consider it as `Identity` functor.

+-----------------------------------------+
| Natural transformations                 |
+----------------+------------------------+
| a -> f b       | Injection              |
+----------------+------------------------+
| f a -> b       | Extraction             |
+----------------+------------------------+
| f a -> g b     | Natural transformation |
+----------------+------------------------+

`fmap` allows us to transform a unary transformation `(a -> b)` to the given
functor i.e. `(f a -> f b)`. However our functions still work only when the
input and output both are within the same functor. Two additional primitives
allow us to address all possible cross functor transformations.

+------------+--------------------------+
| fmap       | (a -> b) -> (f a -> f b) |
+------------+--------------------------+
| pure       | a -> f a                 |
+------------+--------------------------+
| extract    | f a -> a                 |
+------------+--------------------------+

Functors vs Functions
---------------------

A pure transform or unary function is a fundamental operation. It is a map from
one type to another.  However we can represent this function in the form of
data as well! A `representable functor` is just that; an incarnation of a
transform or a function in data form. It is just a fancy term for a map like
data structure. It is a keyed container or a lookup table. A function is just
that; a map or a lookup table.  A representable functor just reifies the
function. Note that, `all functions can be represented as data structures but
not all data structures are functions`.  So those functors that are possible to
be represented as a function are representable functors.

.. Pictures: we can represent a representable functor with a function like box
   with slots. However it will be a double outlined box to represent a functor
   layer.

.. When thinking about representable we can always draw parallels from
   functions in the pure world. Functions are the simplest representable
   functors.

Yoneda Lemma
~~~~~~~~~~~~

A function is a representable functor.

The essence of Yoneda Lemma in category theory is that functions can be
represented as data. Representable functors are data that can be represented as
functions, and a function can be represented by a representable functors as
Yoneda Lemma says. There can be many different representations of a function as
a functor. However they all correspond to the same function when converted to
the function form. Each representation can be converted from one to the other
using a natural transformation i.e by converting the type and fmapping the
functions.

We can say that everything in a Haskell program is a Functor. A function is a
representable functor and data is a traversable functor. The whole program is a
functor that is a composition of functors or an adjunction of functors.

Function to Functor
~~~~~~~~~~~~~~~~~~~

A function application can be represented as the Reader functor.

Let me re-word it in terms of functors and natural transformations. For any
functor f and any type e, all natural transformations of the form::

  forall a . ((e -> a) -> f a)

are in one-to-one correspondence with values of the type `f e`. This is a pretty
powerful equivalence. On the one hand you have a polymorphic function, on the
other hand a polymorphic data structure, and they encode the same data. Except
that things you do with functions are different than things you do with data
structures so, depending on the context, one may be more convenient than the
other.

You can look at this result as the CPS transform: Any function can be encoded
in the Continuation Passing Style. The argument `(e -> a)` is the continuation.
The forall quantifier tells us that the return type of the continuation is up
to the caller.

Functor to Function: Representable functors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A representable functor `f x` is ismorphic to a function which means it can be
represented by a function `g :: r -> x` for some concrete data type `r` where
`r` depends on (is determined by) `f`.

In other words, a representable functor allows us to index into its data
structure using another data type `r` as the index.  The data structure is the
`tabulated` form and the function is the `indexable` form of the same
structure.

A functor is Naperian/Representable iff it's isomorphic to (->) r for some r.
Such a functor can be thought of as a container of a fixed size, where r is the
type of positions in the container. By representing a position as a function of
type forall x. f x -> x, which gets the value at that position, a
Naperian/Representable functor can equivalently be shown to be one for which f
is isomorphic to (->) (forall x. f x -> x)

A Naperian container has one shape, and a set of positions.
So it represents an endofunctor $X \mapsto X^K$.
The logarithm of this container is K.
Every container is a sum of Naperian containers.

To be Naperian is to be a multiplicative, unsummy kind of thing,
all products and exponentials, with no Sigma.

Note, a sum of fixed shape (same shape elements) can be represented as a
product.

From https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/aplicative.pdf:
The necessary additional structure is given by what Hancock calls a Naperian
functor, also known as a representable functor; that is, a container of fixed
shape. Functor f is Naperian if there is a type p of ‘positions’ such that f a
' p → a; then p behaves a little like a logarithm of f —in particular, if f and
g are both Naperian, then Log (f × g) ' Log f + Log g and Log (f · g) ' Log f ×
Log g. ::

  class Functor f ⇒  Naperian f where
  type Log f
  lookup :: f a → (Log f → a)
  tabulate :: (Log f → a) → f a
  positions :: f (Log f )

Informally, Log f is the type of positions for f ; lookup xs i looks up the element
of xs at position i; tabulate h yields an f -structure where for each position i
the element at that position is h i; and positions yields an f -structure where
the element at each position i is i itself. The first two operations should be
each other’s inverses; they are witnesses to the isomorphism between f a and
Log f → a. The latter two operations are interdefinable, so an instance need
only provide one of them; it is often convenient to implement positions, but to
use tabulate. For simplicity, we rule out empty data structures, insisting that
the type Log f should always be inhabited. Naperian functors are necessarily
applicative too::

  pure a = tabulate (λi → a)
  fs ~ xs = tabulate (λi → (lookup fs i) (lookup xs i))

.. Picture

The Representable class implements two functions. `index` converts the functor
`f x` to a function `g :: r -> x` and `tabulate` is the inverse i.e. it
converts a function of type `g :: r -> x` into the functor's representation `f
x`, specifically, ``index . tabulate = id``.

+-----------+----------------------+-----------------+-----------------+
| Operation | Description          | Covariant       | Contravariant   |
+===========+======================+=================+=================+
| tabulate  | Function -> Functor  | (r -> x) -> f x | (x -> r) -> f x |
+-----------+----------------------+-----------------+-----------------+
| index     | Functor  -> Function | f x -> (r -> x) | f x -> (x -> r) |
+-----------+----------------------+-----------------+-----------------+

::

  class Representable f where
     type Rep f :: *
     index    :: f x -> (Rep f -> x)
     tabulate :: (Rep f -> x) -> f x

  data Stream x = Cons x (Stream x)

  instance Representable Stream where
      type Rep Stream = Integer
      index (Cons b bs) n = if n == 0 then b else index bs (n - 1)
      tabulate f = Cons (f 0) (tabulate (f . (+1)))

Which types are representable?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is easy to see that if a functor `f x` has a product type constructor then
the constructor is obviously a function of type `x`.  Intuitively, it should be
possible to find a function that corresponds to the constructor.  However, it
may not always be possible to represent a Sum type functor by a function
because it is a union of data structures some of which may be functions of `x`
and some not. If all the components of a Sum type are representable themselves
then the Sum type is representable.

Co-recursive data types are representable whereas recursive ones are not
because they have a stop condition. Notice that Co-recursive data types are
product types and recursive are Sum types because we need to encode the stop
condition as one of the case.

.. ----------------

Let's do some counting exercises. Product Identity Identity holds exactly two
things. It is therefore isomorphic to ((->) Bool), or if we prefer, ((->)
Either () ()). That is to say that a pair that holds two values of type a is
the same as a function that takes a two-valued type and yields a value of type
a. A product of more functors in turn is isomorphic to the reader of the sum of
each of the datatypes that "represent" them. E.g. Product (Product Identity
Identity) (Product (Const ()) Identity) is iso to ((->) (Either (Either () ())
()), i.e. a data type with three possible inhabitants. In making this move we
took Product to Either -- multiplication to sum. We can pull a similar trick
with Compose. Compose (Product Identity Identity) (Product Identity Identity)
goes to ((->) (Either () (),Either () ())). So again we took Product to a sum
type, but now we took Compose to a pair -- a product type! The intuition is
that composition multiplies the possibilities of spaces in each nested functor.

Hmm.. products go to sums, composition goes to multiplication, etc. This should
remind us of something -- these rules are exactly the rules for working with
exponentials. x^n * x^m = x^(n + m). (x^n)^m = x^(n*m). x^0 = 1, x^1 = x.

Seen from the right standpoint, this isn't surprising at all, but almost
inevitable. The functors we're describing are known as "representable," a term
which derives from category theory. (See appendix on representable functors
below).

In Haskell-land, a "representable functor" is just any functor isomorphic to
the reader functor ((->) a) for some appropriate a. Now if we think back to our
algebraic representations of data types, we call the arrow type constructor an
exponential. We can "count" a -> x as x^a, since e.g. there are 3^2 distinct
functions that inhabit the type 2 -> 3. The intuition for this is that for each
input we pick one of the possible results, so as the number of inputs goes up
by one, the number of functions goes up by multiplying through by the set of
possible results. 1 -> 3 = 3, 2 -> 3 = 3 * 3, (n + 1) -> 3 = 3 * (n -> 3).

Hence, if we "represent" our functors by exponentials, then we can work with
them directly as exponentials as well, with all the usual rules. Edward Kmett
has a library encoding representable functors in Haskell.

Meanwhile, Peter Hancock prefers to call such functors "Naperian" after John
Napier, inventor of the logarithm (See also here). Why Naperian? Because if our
functors are isomorphic to exponentials, then we can take their logs! And that
brings us back to the initial discussion of type mathematics. We have some
functor F, and claim that it is isomorphic to -^R for some concrete data type
R. Well, this means that R is the logarithm of F. E.g. (R -> a, S -> a) =~
Either R S -> a, which is to say that if log F = R and log G =~ S, then log (F
* G) = log F + log G. Similarly, for any other data type n, again with log F =
R, we have n -> F a =~ n -> R -> a =~ (n * R) -> a, which is to say that log
(F^n) =~ n * log F.

This gives us one intuition for why the sum functor is not generally
representable -- it is very difficult to decompose log (F + G) into some
simpler compound expression of logs.)

So what functors are Representable? Anything that can be seen as a fixed shape
with some index. Pairs, fixed-size vectors, fixed-size matrices, any nesting of
fixed vectors and matricies. But also infinite structures of regular shape!
However, not things whose shape can vary -- not lists, not sums. Trees of fixed
depth or infinite binary trees therefore, but not trees of arbitrary depth or
with ragged structure, etc.

Representable functors turn out to be extremely powerful tools. Once we know a
functor is representable, we know exactly what its applicative instance must
be, and that its applicative instance will be "zippy" -- i.e. acting pointwise
across the structure. We also know that it has a monad instance! And,
unfortunately, that this monad instance is typically fairly useless (in that it
is also "zippy" -- i.e. the monad instance on a pair just acts on the two
elements pointwise, without ever allowing anything in the first slot to affect
anything in the second slot, etc.). But we know more than that. We know that a
representable functor, by virtue of being a reader in disguise, cannot have
effects that migrate outwards. So any two actions in a representable functor
are commutative. And more than that, they are entirely independent.

This means that all representable functors are "distributive"! Given any
functor f, and any data type r, then we have::


  distributeReader :: Functor f => f (r -> a) -> (r -> f a)
  distributeReader fra = \r -> fmap ($r) fra

That is to say, given an arrow "inside" a functor, we can always pull the arrow
out, and "distribute" application across the contents of the functor. A list of
functions from Int -> Int becomes a single function from Int to a list of Int,
etc. More generally, since all representable functors are isomorphic to reader,
given g representable, and f any functor, then we have: distribute :: (Functor
f, Representable g) => f (g a) -> g (f a).

This is pretty powerful sauce! And if f and g are both representable, then we
get the transposition isomorphism, witnessed by flip! That's just the beginning
of the good stuff. If we take functions and "unrepresent" them back to functors
(i.e. take their logs), then we can do things like move from ((->) Bool) to
pairs, etc. Since we're in a pervasively lazy language, we've just created a
library for memoization! This is because we've gone from a function to a data
structure we can index into, representing each possible argument to this
function as a "slot" in the structure. And the laziness pays off because we
only need to evaluate the contents of each slot on demand (otherwise we'd have
a precomputed lookup table rather than a dynamically-evaluated memo table).

And now suppose we take our representable functor in the form s -> a and paired
it with an "index" into that function, in the form of a concrete s. Then we'd
be able to step that s forward or backwards and navigate around our structure
of as. And this is precisely the Store Comonad! And this in turn gives a
characterization of the lens laws.

What this all gives us a tiny taste of, in fact, is the tremendous power of the
Yoneda lemma, which, in Haskell, is all about going between values and
functions, and in fact captures the important universality and uniqueness
properties that make working with representable functors tractable. A further
tiny taste of Yoneda comes from a nice blog post by Conal Elliott on
memoization.

Which types are Distributive?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are three related ways of looking at it.

Product vs Sum
..............

An easy way to remember is the way multiplication distributes but addition does
not, the same way product types distribute but sum types do not. Functions are
products so they are distributive, since representable functors are
representations of functions they are distributive.

Producer vs Consumer
....................

For it to always be able to consume a value, all constructors of a distributive
functor must be isomorphic to a function using the same type argument or in
other words we must be able to represent the sum as product.  `Maybe` is not
distributive because the constructor `Nothing` is not isomorphic to a function.
Exercise, is `Either` Distributive?

Representable vs Not Representable
..................................

Note that something that is not Representable is not Distributive and
vice-versa. Therefore Sum types, in general,  are not Distributive.

Representable functors are isomorphic to the reader monad and therefore all
representable functors are monads themselves.

Every `Distributive` functor is isomorphic to `r -> _` for some `r` and
therefore `Representable`.  To be distributable a container needs to have a way
to consistently zip a potentially infinite number of copies of itself
(comonoid).

.. Relation with comonoids?

Memoization and Performance
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A representable functor gives us two different implementations of the same
thing — a function and a data structure, both representing exactly the same
content. This allows us the opportunity to use memoization on the function
representation and use random access to improve performance.

We can also achieve laziness by converting any indexable data structure into
its function form.

The `adjunctions` package provides `Reader`, `State` monads and `Store` comonad
implementations based on Representable functors, utilizing memoization.

Contravariant Representable
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Representable contravariant functors are isomorphic to `(_ -> r)` for some
representation type `r` and resemble mappings to a fixed range.

.. Picture

::

  class Representable f where
     type Rep f :: *
     index    :: f x -> (x -> Rep f)
     tabulate :: (x -> Rep f) -> f x

Combining Functors
------------------

Functor Composition
~~~~~~~~~~~~~~~~~~~

Functor Types that are composition of other Functor Types
(base, Data.Functor.*):

+----------+------------+------------+
| Sum      | Product    | Compose    |
+----------+------------+------------+

Distributive Composition
~~~~~~~~~~~~~~~~~~~~~~~~

.. f $ g = ($ g) f

A function distributes over a functor.

.. A representable functor (a function) distributes over a traversable functor (a
   container of arguments), just like multiplication distributes over addition.

..
  There are common situations when we want to operate on two nested containers.
  For example given a list of IO actions we want to collect the outputs of all IO
  actions and put list of results in IO container. Essentially we swap the
  nesting order of the container. This is known as a distributive operation,
  quite similar to the elementary maths distribution switching the nesting of
  addition and multiplication::

    1 * (2 + 3) = (1 * 2) + (1 * 3)

  ::

    map putStrLn ["hello", "world"] :: [IO (), IO ()]
    sequence $ map putStrLn ["hello", "world"] => IO [(), ()]

    [putStrLn, print] :: [String -> IO (), String -> IO ()]
    map ($ "hello") [putStrLn, print] :: [IO (), IO ()]
    sequence $ map ($ "hello") [putStrLn, print] => IO [(), ()]

    distribute [(+2), (*2)] 1 :: [Int]
    distribute [putStrLn, print] "hello" :: [IO (), IO ()]
    sequence $ distribute [putStrLn, print] "hello" :: IO [(), ()]

Consider nested functors `f (g a)`. There are two dual ways to distribute this
to arrive at `g (f a)`. Either, the outer functor `f` is foldable (also
finitary) and inner functor `g` applicative (i.e. the values in `g` can be used
as arguments to functions) or the outer functor `f` is duplicatable (possibly
infinite) and the inner functor `g` is representable (i.e. the values in `g`
can be used as functions).  Conceptually, in the first case we fold the values
in the outer container and reconstruct it inside the inner container using
applicative. In the latter case we duplicate the outer container and then zip
the modified copies.  The former case is captured by the `Traversable`
typeclass and the latter by the `Distributive` typeclass.

.. In the first case we "fold" and "construct" and in the second case we "copy"
   and "zip". The first one works on finite containers and the second one can
   work on infinite containers too.

.. Zippability is about compatibility of shapes of containers. Traversability is
  about finitude of containers' element position sets. Infinite streams have a
  sensible zipWith operator, but are not Traversable.

.. representation theorem can be used to prove that traversable functors are
  finitary containers, how coalgebras of a parameterised store comonad relate
  to very well-behaved lenses.

.. Well, with the help of universe, one could potentially write Foldable and
  Traversable instances for state transformers over finite state spaces. The
  idea would be roughly similar to the Foldable and Traversable instances for
  functions: run the function everywhere for Foldable and make a lookup table
  for Traversable. Thus:
  https://stackoverflow.com/questions/32812400/are-there-non-trivial-foldable-or-traversable-instances-that-dont-look-like-con/32812758#32812758

There is another way to think about it, in both cases a function application
occurs, however, in case of `traverse` a function goes to values that can act
as arguments and in the case of `distribute` an argument goes to values that
can act as functions.

Traversable Functors
^^^^^^^^^^^^^^^^^^^^

The traversable typeclass allows distribution of nested containers when the
outer container is traversable (finite and foldable) and the inner container is
applicative. The `sequence` operation inverts the nesting order of the
containers. The `traverse` operation applies a function (injection style i.e.
``a -> f b``) to the elements in container ``t a`` and collects the results as
``f (t b)``.

+-----------------------------------------------------------------------------+
| sequence :: (Foldable t, Functor t, Applicative f) => t (f a) -> f (t a)    |
+-----------------------------------------------------------------------------+

::

  -- t is folded, contrast with Distributive where f is distributed
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

  -- same as collect
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

Distributive Functors
^^^^^^^^^^^^^^^^^^^^^

The `Distributive` typeclass allows distribution of nested containers when the
outer container is distributive (duplicatable and zippable, which is true for
all functors in Haskell) and the inner container is representable
(function-like). The `distribute` operation inverts the nesting order of the
containers, in other words it distributes its argument to the functions in the
container.

.. Notice that in case of Traversable it is easier to explain traverse than
  sequence. In case of Distributive it is easier to explain distribute than
  cotraverse. Duality!

+-----------------------------------------------------------------------------+
| distribute :: (Functor t, Distributive f) => t (f a) -> f (t a)             |
+-----------------------------------------------------------------------------+

In the following example the argument ``1`` is supplied to all the functions in
the list, resulting in an output ``[2, 2]``::

  (distribute [(+1), (*2)]) 1

.. Picture

..
  -- For the previous example, the types will be
  -- t :: [], f :: (->) Int, a :: Int
  -- distribute :: [Int -> Int] -> (Int -> [Int])

.. Due to the lack of non-trivial comonoids in Haskell, we can restrict ourselves
  to requiring `t` to be a Functor rather than some Coapplicative class.

.. Illustrate duality with Traversable. Distributive is the inner functor
    whereas traversable is the outer functor. Distributive implies representable,
    traversable requires foldable. We fold a traversable containing applicatives,
    we distribute a Distributive inside a coapplicative (functor actually).

::

  -- f is distributed, contrast with traversable where t is folded
  distribute  :: (Functor t, Distributive f) => t (f a) -> f (t a)

  -- same as traverse, a distributive can be traversed as well of course, since
  -- it is a product type, it allows both, it is a sum as well.
  collect     :: (Functor t, Distributive f) => (a -> f b) -> t a -> f (t b)
  collect f   = distribute . fmap f

  cotraverse  :: (Functor t, Distributive f) => (t a -> b) -> t (f a) -> f b
  cotraverse f = fmap f . distribute

We can zip two distributive containers by distributing the Pair Functor::

  data Pair a = Pair a a deriving Functor

  zip :: Distributive f => f a -> f a -> f (a, a)
  zip xs ys = fmap f $ distribute (Pair xs ys)
    where f (Pair x y) = (x, y)

Commutative Composition
~~~~~~~~~~~~~~~~~~~~~~~

A generalization of `f . g = g . f` to representable functors.

Transposition in general consumes an f -structure of g-structures in which all
the g-structures have the same shape, and produces a g-structure of f -structures
in which all the f -structures have the same shape, namely the outer shape of
the input. For general functors f and g, this is a partial function, or at best
a lossy one. However, the essential point about Naperian functors is that all
inhabitants of a datatype have a common shape. In particular, in an f -structure
of g-structures where both f and g are Naperian, all the inner g-structures
necessarily have the same (namely, the only possible) shape. Then transposition
is total and invertible::

  transpose :: (Naperian f , Naperian g) ⇒ f (g a) → g (f a)
  transpose = tabulate · fmap tabulate · flip · fmap lookup · lookup

As a consequence, composition of Naperian functors is commutative, up to isomorphism;
we will insist on our dimensions being at least Naperian functors.
Of course, pairs are Naperian, with two positions—the usual ordering on
booleans in Haskell has False 6 True, so we use this ordering on the positions
too::

  instance Naperian Pair where
    type Log Pair = Bool
    lookup (P x y) b = if b then y else x
    positions = P False True

Adjunctions: Applying Transformations to Containers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An adjunction is a "functor application" i.e. generalization of function
application to representable functors. A representable functor is the function
and a traversable functor is its argument.

Consider two functors `t` and `f` such that `t` is a finitary traversable
container and `f` is a representable functor. In other words `t` is pure data
container and `f` is a representation of a transformation (can be nested). An
adjunction describes how to combine these two together. That is, extract
elements from the container `t` and apply them to the representable container
`f`. Put another way an adjunction is an abstract notion that represents a
function application using an argument from a container and applying it to a
function. ::

  class (Functor t, Representable f) => Adjunction t f | t -> f, f -> t where
    unit         :: a -> f (t a)
    counit       :: t (f a) -> a

    unit . counit :: t (f a) -> f (t a) -- distributive

.. We have factored distribute in two halves i.e. distribute = unit . counit

`t` is called the left adjoint and `f` the right (mnemonic `right` is
`representable`) adjoint functor. `f` is the function and `t` is the
traversable container. The relation between them is asymmetric just like a
function application is asymmetric. `t` is a free functor and `f` is a
forgetful functor.

leftAdjunct and rightAdjunct form two halves of an isomorphism i.e.  ``(t a ->
b)`` and ``(a -> f b)`` are isomorphic.

::

    leftAdjunct  :: (t a -> b) -> (a -> f b)
    leftAdjunct f  = fmap f . unit

    rightAdjunct :: (a -> f b) -> (t a -> b)
    rightAdjunct f = counit . fmap f

    unit           = leftAdjunct id
    counit         = rightAdjunct id

    rightAdjunct unit = id
    leftAdjunct counit = id

Function `t a -> b` reduces the container `t a` to type `b`.  `a -> f b`
applies a single element of type `a` to the representable functor `f`. If `t`
and `f` are adjunct that means for a `t a -> b` there exists an equivalent `a
-> f b` and vice-versa. This means we can add the elements in the container one
at a time to representable functor `f`.

For a concrete example think about the adjunction where `t` is a tuple type and
`f` is a function type, therefore a function currying is `a -> f b` and an
uncurried application is `t a -> b`. The adjunction means we can convert a
curried function into uncurried form and vice-versa. In other words, it means
that `t` and `f` have a distributive relation, that is `t (f a)` can be
converted to `f (t a)`.

..  Every pair of adjoint functors defines a monad and a comonad. Conversely,
    every monad or comonad may be factorized into a pair of adjoint functors —
    this factorization is not unique, though.
    Every Adjunction F -| G : C -> D, gives rise to a monad GF on D and a
    Comonad FG on C.

Free Construction
-----------------

* https://hackage.haskell.org/package/free-functors

You store the value and the map function in a data structure.

::

  -- g is a type function with one parameter i.e. of kind 'Type -> Type'
  data Lan g a where
    Lan :: g x -> (x -> a) -> Lan g a

  instance Functor (Lan g) where
    fmap k (Lan x f) = Lan x (f . h)

  lan :: g a -> Lan g a
  lan x = Lan x id

References
----------

* https://bartoszmilewski.com/2014/01/14/functors-are-containers/

Traversable
~~~~~~~~~~~

* http://r6.ca/blog/20121209T182914Z.html On the Static Nature of Traversals
* http://ropas.snu.ac.kr/~bruno/papers/Iterator.pdf The Essence of the Iterator
  Pattern
* https://arxiv.org/pdf/1202.2919.pdf An Investigation of the Laws of
  Traversals
* http://www.cs.ox.ac.uk/jeremy.gibbons/publications/uitbaf.pdf Understanding
  Idiomatic Traversals Backwards and Forwards
* https://github.com/oconnorr/traversable-fincontainer
* https://arxiv.org/pdf/1402.1699.pdf A Representation Theorem for Second-Order
  Functionals
* https://stackoverflow.com/questions/32812400/are-there-non-trivial-foldable-or-traversable-instances-that-dont-look-like-con/32812758#32812758

Representable
~~~~~~~~~~~~~
* http://sneezy.cs.nott.ac.uk/containers/blog/?p=14 What is a Naperian
  container?
* https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/aplicative.pdf
  APLicative Programming with Naperian Functors
* https://tonyday567.github.io/naperian/
* http://comonad.com/reader/2013/representing-applicatives/
* https://stackoverflow.com/questions/12963733/writing-cojoin-or-cobind-for-n-dimensional-grid-type/13100857#13100857
* http://chrispenner.ca/posts/representable-cofree-zippers
* http://chrispenner.ca/posts/adjunction-battleship
* http://chrispenner.ca/posts/free-forgetful-functors
* http://chrispenner.ca/posts/representable-discrimination
* https://bartoszmilewski.com/2015/07/29/representable-functors/
* https://en.wikipedia.org/wiki/Representable_functor
* https://medium.com/@drboolean/laziness-with-representable-functors-9bd506eae83f
* http://blog.sigfpe.com/2009/11/memoizing-polymorphic-functions-with.html
* http://conal.net/blog/posts/memoizing-polymorphic-functions-via-unmemoization
* https://www.reddit.com/r/haskell/comments/2y25ly/language_deriveapplicative/

Others
~~~~~~

* https://bartoszmilewski.com/2013/05/15/understanding-yoneda/
* https://bartoszmilewski.com/2013/10/08/lenses-stores-and-yoneda/
* http://www-kb.is.s.u-tokyo.ac.jp/~asada/papers/arrStrMnd.pdf Arrows are
  Strong Monads

Packages
--------

* base

..  Data.Functor
    Data.Functor.Classes
    Data.Functor.Compose
    Data.Functor.Const
    Data.Functor.Identity
    Data.Functor.Product
    Data.Functor.Sum

* http://hackage.haskell.org/package/naperian Efficient representable functors
* http://hackage.haskell.org/package/representable-tries function memoization
* https://hackage.haskell.org/package/natural-transformation
