A Type
------

Any type creates an indirection or a context in which the value is to be
interpreted. It says you cannot directly operate on me, you will have to
use my way of doing things. It enforces manipulation rules. The rules
followed by the type are specified by the typeclass.

Functor
-------

A type is a functor if it knows how to apply a function to the type
or value wrapped in it. A type has its own unique way of applying the
function. Different functors can apply the function in different ways.

fmap      :: (a -> b) -> f a -> f b  -- covariant
contramap :: (a -> b) -> f b -> f a  -- contravariant

data WithInt a = WithInt (Int -> a)  -- covariant on a
data MakeInt a = MakeInt (a -> Int)  -- contravariant on a

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

Examples
^^^^^^^^

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

Applicative
-----------

Also called "strong lax monoidal functor". The monoidal formulation is
more elegant. Apply a function (functor property) and combine (monoidal
property).

class (Functor f) => Applicative f where
  pure :: a -> f a
  zip :: (f a, f b) -> f (a, b)

A functor type allows you to have function objects wrapped in that type,
but it does not know how to apply them to values wrapped in the same
type. Applicative adds that via <*>. An applicative type provides a type
specific way of applying functions contained in that type to values
contained in that same type.

<*> :: f (a -> b) -> f a -> f b

This is another way of composing analogous to function application.

Examples
^^^^^^^^

List: apply a collection of functions on a collection of values and
combine the results. Its own unique way of application - apply each
function to each value and then concatenate the results.

>> [id,id,id] <*> [1,2,3]
[1,2,3,1,2,3,1,2,3]

IO: Apply the function to the values resulting from the IO action. Note
the function itself is NOT an IO action or something resulting from an
IO action.

sz <- (++) <$> getLine <*> getLine

Maybe:

Monad
-----

A Monad knows how to flatten the same type contained within the same
type. join eliminates a layer of indirection, the elimination is encoded in a
type specific manner:

join   :: M (M a) -> M a

It allows functions of type (a -> m b) to be mapped to the type and results
collected by joining. Join behavior defines the Monad.

(>>=) :: Monad m => m a -> (a -> m b) -> m b
m >>= g = join (fmap g m)

Examples
^^^^^^^^

List: join is concatenation of the resulting list of lists:
xs >>= f = concat (map f xs) -- concat == join

IO: join is strict evaluation of the IO action (case is strict):

bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s
join x   = x >>= id

Note that for IO '<*> = ap', ap is defined in terms of monadic
primitives and has a Monad constraint on the type, so even the
applicative sequencing will also strictly evaluate the IO actions in
sequence.

Pitfalls: Remember, a monad is much more than IO and IO is much more
than a Monad.

Comonad
-------

Functor/Applicative/Monad As composition mechanisms
---------------------------------------------------

We just use the basic primitives (functions) and apply them in many different
ways depending on the type i.e. the behavior is a function of the type on which
we are applying them. Intuitions:

* A function with an argument transforms its argument
* A Functor allows us to apply a transform to that transform. That is
we are able to use a normal function in a different way depending on
the Functor type i.e. we are combining the peculiar behavior of the type to an
existing function. Pitfall - do not stereotype the concept by thinking only about a
collection type like list.
* A normal function could be wrapped in a (applicative) type and applied
to values wrapped in the same type. This is another way of composing
which is a transformation on normal function application to the peculiar
world of the type.
