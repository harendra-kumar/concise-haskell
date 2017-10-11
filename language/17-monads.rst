Terminology
-----------

Monads is the most general composition of functors. Monads have the full
power of functions in the pure world. They allow something like
continuations for functors.  They allow product-like composition i.e. a
tree structured composition. They are like the case analysis for
functors. We draw from a functor the way we draw from an argument in a
pure function and then compose it with something drawn from another
functor object. Bind is the cross product.

Monads generalization of CPS?
-----------------------------

"Recently (1989) Moggi has shown how monads, a notion from category theory,
generalise the continuation-passing style transformation"

Syntax
------

Monad:
    parseTerm = do
         x <- parseSubterm
         o <- parseOperator
         y <- parseSubterm
         return $ Term x o y

Arrow: (the only difference from Monad is the static input at the tail)
    parseTerm = proc _ -> do
         x <- parseSubterm -< ()
         o <- parseOperator -< ()
         y <- parseSubterm -< ()
         returnA -< Term x o y

Applicative:

    parseTerm = Term <$> parseSubterm <*> parseOperator <*> parseSubterm

Monad
-----

A Monad knows how to flatten the same type contained within the same
type. join eliminates a layer of indirection, the elimination is encoded in a
type specific manner::

  join   :: M (M a) -> M a

It allows functions of type (a -> m b) to be mapped to the type and results
collected by joining. Join behavior defines the Monad::

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  m >>= g = join (fmap g m)

Examples
~~~~~~~~

List: join is concatenation of the resulting list of lists::

  xs >>= f = concat (map f xs) -- concat == join

IO: join is strict evaluation of the IO action (case is strict)::

  bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s
  join x   = x >>= id

List Monad Desugaring
~~~~~~~~~~~~~~~~~~~~~

In a list Monad bind is equivalent to `foreach`.

Single argument function::

  do
    x <- [1,2,3]
    return x

  [1,2,3] >>= return

Multi argument function::

  do
    x <- [1,2,3]
    y <- [3,4,5]
    return (x, y)

  [1,2,3] >>= (\x1 -> [3,4,5] >>= (\x2 -> return (x1, x2)))

  The first bind will result in a list:
    [3,4,5] >>= (\x2 -> return (1, x2)
    [3,4,5] >>= (\x2 -> return (2, x2)
    [3,4,5] >>= (\x2 -> return (3, x2)

  Notice that in a multiargument function the first argument is bound first.
  The order may be important depending on the semantics of the monad, for
  example in the IO monad (or any effects monad) the order is important.

  The first bind results in 'foreach x', the second one in 'foreach y'
    foreach x
      foreach y
        return list element (x, y)

Notice how the list monad looks quite like a mini DSL by hiding the details
under the hood.

Monad Intuition
---------------

Monad as Interpreter
~~~~~~~~~~~~~~~~~~~~

A Monad is actually an interpreter.  Each Monad interprets the actions being
performed in it in its own way, and therefore creates a DSL. For example the IO
Monad sequences them, the Maybe monad composes them with error handling
semantics, a list monad combines all elements of a list etc.

The semantics of the interpreter are built into the bind operation.

Bind is a special type of composition which allows you to intercept the
composition and do something extra before passing the value to the next
function.

A regular function application is defined by the runtime system. In a monad the
application is user defined. However the application is enforced to be one
application at a time. In a function application the order of apply is not
defined, they can happen in parallel. However in a monad the order of each
application is fully defined. That is the difference between an identity monad
and pure function application. Identity monad specifies apply order though that
should not be confused with evaluation, it does not guarantee evaluation unless
explicitly enforced by bind implementation.

Even when a function has multiple arguments which one is applied first is
specified by creating lambdas and the do notation helps in doing that
conveniently. Refer to how we created lambdas to curry the arguments out of
order.

The free monad uses a data structure which specifies the application order
which allows us to implement the bind operation later. That's why a free monad
looks like a list, it specifies a sequence.

Monad as Indirection
~~~~~~~~~~~~~~~~~~~~

"We can solve any problem by introducing an extra level of indirection." -
David J. Wheeler.

* Functor is needed to create the type indirection. Functor just helps you
  create correspondences between any types and the indirection. So that you can
  operate on any type in a special mapped world of the functor.
* pure/return just lets you put a value from any regular type into the
  indirection layer so that we can operate on it in the modified composition
  environment. Since applicative and monad are general composition techniques
  we need to put values inside a functor to use the composition. pure/return
  guarantee that we have a way to do that. It lifts a pure value into the type.
* An applicative creates a direct correspondence of a pure function application
  and composition inside the functor.

::

          Functor
            |
            v
       pure/return = guarantee ability to inject values into the functor
            |
            v
        Applicative
            |
            v
          Monad

fmap + return = a -> f b (pure function)

* although Applicative and Functor can be expressed in terms of Monad, they
  still have to be written explicitly.
* liftM is to monad like fmap is to functor

A type introduces a layer of indirection. An indirection allows an abstraction.
A functor type is a very useful indirection as it allows a direct mapping from
any type.

We can perform operations at a layer of indirection. For example, pure or
return can do something when it puts a pure element into the indirection or the
box.  Similarly a monad can do something when it combines two indirections.

With monads as well we have introduced an indirection and do things under the
hood inside the indirection.  Another way of thinking about this is that we
have created boxes around the values, we deal with values and not the boxes.
What is done when these boxes are joined is what defines a monad. For example,
in an IO monad the boundaries of the boxes introduce strict evaluation.

Monad combines functions and does something special when the functions are
composed. Therefore we can use the indirection to pass an invisible state
across all the functions when they are composed. Here the function of the
indirection is handover of the state from one guy to another.

The two tracks
~~~~~~~~~~~~~~

We can also think of monads as compartmentalising our functions inside functor
boxes. Now we have two independent layers. One layer that composes those boxes
together is the monad bind layer. One our regular computation within those
boxes. It is like two tracks running in parallel one is the main track and the
other is auxiliary track behind the scenes. It is like the checked luggage
which arrives when you reach the destination, you do not know how its
transported you just receive it. The luggage could be the shared state.

Another way to think about it is multiple return values. A side effect
producing function actually has more than one return values, the regular pure
return value and a side effect. And we need to compose both. One track composes
the pure value and the other track handles the side effects. Side effects can
be sequenced via composition. In IO monad sequencing is one track and passing
the IO values is another track.

Passing State
-------------

In a pure functional programming paradigm there are no global variables or
pointers. Functions are pure so how do we work on global state or pass state
down to a deeply embedded function. The only way to pass values is via
arguments and that's how we do it. Monads allow us to separate the state
passing functions from the pure functions. A monad is a chained computation
where state is handed over from the previous function to the next. The state
passing is hiddden from the user of the monad, the user can use pure functions,
examine or change the state and the state will be passed on made available at
any point via the moand.

In a continuation passing style we can build higher level functions by
composing functions. The arguments of a function can be used to create the next
function in the chain. Therefore CPS is a pretty common (or necessary) style
used in monad implementations where state has to be passed around.

Example:

Also see the transformers chapter for more details on state passing monads.

Standard Monads
---------------

* A monad is strict if its >>= operation is strict in its first argument. That
  means it evaluates the result of the previous action before passing it on to
  the next action.

+-----------------------------------------------------------------------------+
| Basic monads defined in the `base` package                                  |
+----------+---------+--------------------------------------------------------+
| Name     | Strict? | Monadic semantics                                      |
+==========+=========+========================================================+
| Identity |         | No additional semantics, just like pure functions      |
|          |         | bind is just a function application.                   |
+----------+---------+--------------------------------------------------------+
| Function |         | Supplies the original value along with the result to   |
|          |         | the next function.                                     |
+----------+---------+--------------------------------------------------------+
| Maybe    | Strict  | Passes on the Just value, stops when it sees Nothing.  |
+----------+---------+--------------------------------------------------------+
| Either   | Strict  | Passes on the right value, stops when it sees Left     |
+----------+---------+--------------------------------------------------------+
| []       | Strict  | Applies every action to all elements of the list       |
+----------+---------+--------------------------------------------------------+
| IO       | Strict  | Evaluate previous action before performing the next.   |
+----------+---------+--------------------------------------------------------+
| ST       | Strict  | Evaluate previous action before performing the next.   |
|          +---------+--------------------------------------------------------+
|          | Lazy    |                                                        |
+----------+---------+--------------------------------------------------------+

Effectful Monads (IO & ST)
--------------------------

A pure function has an explicit and only one output. An effectful function has
a pure output and an effect. The output can be collected, folded, processed
further etc.  Effects are just effects you do not collect them or operate on
them. But there is an operation that is important for effects and that is
"sequence". You can control in what sequence will those effects happen.

IO and ST monads are special in one aspect, they can represent side effects. An
effect can be an IO action or mutating the state of environment in such a way
that can implicitly affect any future operations.

The first requirement for effects is that the monad must be strict i.e. we
evaluate every action completely before we evaluate the next. The strict
evaluation makes sure that any future operations can take the previous effect
into account, or in other words can see the effect. A monad helps us do that by
implementing strictness as the underlying semantics of the monad.

However, we can have pure effect operations which do not generate any explicit
output like a pure function (e.g. a print IO statement). The bind operation of
a monad requires an explicit result from the previous operation to compose the
actions together and implement its semantics.  Pure effects are represented by
a monad by faking an output under the hood even when there is none by using a
`realworld` token.

Open World Effects: The IO Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Strictness ensures that effects `can be` sequenced. However, in case of IO we
also need to enforce that _all_ possible effects are explicitly sequenced with
respect to each other. This is required because the whole world impacted by IO
is considered one global state or one global environment. Therefore all changes
to that global environment must be sequenced.

This is achieved by having the IO monad as a one way type that is you
cannot take values out of it and use them in pure code.  Therefore all IO
actions are guaranteed to be chained or composed together. If we allowed taking
values out of the IO monad then we can go perform some IO effect from pure code
without knowing about it. It creates two problems, (1) there won't be a
guarantee that pure code is really pure, and (2) effects can be performed out
of order with respect to any other effectful operations producing unpredictable
results. Note that this behavior of IO has nothing to do with a monad type. The
one way street is implemented by not exposing the IO constructor and therefore
not allowing a pattern match on it.

Closed World Effects: The ST Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The IO monad assumes an open world i.e.  the state that it operates on is
global for all IO operations. However, the ST monad is designed for closed
world effects i.e. effects are limited to a known isolated state, for example a
mutable array.  Using ST, a pure operation can internally be implemented using
many effectful operations on a closed state.  The ST monad isolates effectful
operations inside it but has a pure interface to the rest of the world.  The
type system (existential quantification) ensures that the internal mutable
state cannot leak out.

+----------+---------+--------------------------------------------------------+
| IO       | Strict  | Evaluate previous action before performing the next.   |
+----------+---------+--------------------------------------------------------+
| ST       | Strict/ | * Embed an opaque mutable data                         |
|          | Lazy    | * Do not allow extraction of the data                  |
+----------+---------+--------------------------------------------------------+

IO Actions - Applicative vs Monad
---------------------------------

IO is an Applicative as well as a Monad, you can use whichever you need.
Applicative and Monad are both ways to compose the sequencing of IO actions.

An applicative is more rigid as the sequencing of actions is tied with function
application. The applicative instance defines the semantics of the side effects
generated by an applicative.

A monad is more flexible, it provides full control of sequencing in the hands
of the programmer. Sequencing of side effects and function applications are
tied together, they can be performed independently, providing more power and
flexibility.

You can think of IO Monad as specifying data dependencies just like an
imperative program has implicit data dependencies. A Monad specifies the
dependencies explicitly.

You can express effectful sequencing using Applicative whereas you can express
effectful looping only using Monads.

Free Monad
----------

However the free monad detaches the semantics from the bind operation and makes
it a more abstract operation. The semantics are added separately by walking
through the composed structure and interpreting it.

A Monad mixes the structure and the custom DSL interpreter together. A free
monad is more modular, it provides only the structure, the interpreter is added
as a separate layer.  Free monads arise every time an interpreter wants to give
the program writer a monad, and nothing more. If you are the interpreter and I
am the program writer, you can push against me and keep your options as free as
possible by insisting that I write a program using a free monad that you
provide me. The free monad is guaranteed to be the formulation that gives you
the most flexibility how to interpret it, since it is purely syntactic.

A Free monad is a data type which is constructed using a Functor. It has all
the properties of Applicative and Monad without actually defining any explicit
natural transformations like <*> or >>=. These transfomrations can be
generically defined for the Free data structure which includes a functor.

A free monad does not have a handling customized for a specific type but it is
a monad. That is, it is a bare minimum monad without any custom semantics::

  data Free f a = Pure a | Free (f (Free f a))

f is a functor. This is a recursive data structure which keeps adding one layer
of functor every time. In our earlier definition of a monad we keep eliminating
the extra layer using ``join``. Here we keep that layer built into the data
structure and eliminate them at one go later when we consume this data
structure.

It is worth noting that free is a recursive sum type dual to cofree. cofree is
a corecursive product type.  The structure of ``Free`` is like a linked list,
adding nested layers of functors which are to be joined later using custom
semantics::

  Free (f (Free (f ... (Free (f (Pure a))))))

A list is just a special case of a free monad, in fact it is a free monoid. In
the following type, the Pure value is ``()`` and the functor is a tuple of
a value of some type ``a`` and the next ``Free`` monad structure. Thus each
layer of the nested functors embed a value of type ``a``::

  type List a = Free ((,) a) ()

The Free monad structure itself is constrained rather than using natural
transformations for constraints to make it a monad.

+-------------------------------------+---------------------------------------+
| Monad                               | Free Monad                            |
+=====================================+=======================================+
| The operations are written so as to | Computations are automatically        |
| conform to a monadic structure      | structured by embedding them in       |
|                                     | an explicit conforming data structure |
+-------------------------------------+---------------------------------------+
| We have to follow the laws          | The structure ensures the laws        |
+-------------------------------------+---------------------------------------+
| Monad semantics are built along     | Semantics are added as a separate     |
| with the operations                 | layer (the interpreter)               |
+-------------------------------------+---------------------------------------+
| More efficient                      | Indirection always comes at a cost.   |
|                                     | Free monads do not come for  free!    |
|                                     | Though the cost may not always be     |
|                                     | significant.                          |
+-------------------------------------+---------------------------------------+

Free vs Cofree
--------------

::

    data Free f a = Pure a | Free (f (Free f a))

    It has a recursive structure. Just like a finite list. Each layer of
    functor can embed values of some type, just as we saw in case of a list
    above, until we reach the base case.

    Free (f (Free (f ... (Free (f (Pure a))))))

    data Cofree f a = a :< f (Cofree f a)

    It has a corecursive structure. Just like an infinite stream. Here there is
    no base case and a value is explicitly embedded in each layer.
    :< a (f (:< a (f (:< a (f (...))))))


Free and Cofree Transformers
----------------------------

Free monad transformer::

  -- | The base functor for a free monad.
  data FreeF f a b = Pure a | Free (f b)
  newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }
  m (Free (f (m (Free (f ... (m (Free (f (m (Pure a))))))))))

  It has multiple layers of functors to get to the base case.

  data CofreeF f a b = a :< f b
  newtype CofreeT f w a = CofreeT { runCofreeT :: w (CofreeF f a (CofreeT f w a)) }
  w (:< a (f (w (:< a (f (w ...))))))

Freer Monad
-----------

::

     data FFree g a where
       FPure   :: a -> FFree g a
       FImpure :: g x -> (x -> FFree g a) -> FFree g a

Notice the structure of FImpure, a function application coupled with a function
generating `FFree g a`.

Monad vs Comonad
----------------

A monad and comonad compose functions whose input end or the output end is
structured by a functor (a -> m b or w a -> b). A monad composes a -> m b, b ->
m c to a -> m c whereas a comonad composes w a -> b, w b -> c to w a -> c.

A monad structures the computation at the output end (a -> m b). The input end
is open. In a monad all monadic computations being combined  must have the
structure m at the output side, their outputs are collapsed or joined by the
rules of m.  On the other hand a comonad structures the input end (w a -> b),
the output end is open. The input of all the comonadic computations being
combined is derived from the same fixed structure w.

In a comonad we start with some existing or "full" state (or a full comonoid)
in w and the state keeps changing as we compose actions, the new state is
decided by the comonad duplicating logic, consuming the side effect and
producing a new state.
In a monad we start with an empty (or empty Monoid) output state and the output
state keeps changing as we compose actions, the new state is decided by the
monad joining logic. That way there is not much difference between a monad and
comonad except the fact whether the starting point and fusion point is before
the composition or after.

In other words, in a Monad the side track is a Monoidal structure at the output
end. In a comonad the side track is a comonoidal structure at the input end of
the composition.  Monoidal structure is recursive, because we have to have a
terminal state?  Comonoidal structure is corecursive because there has to be an
initial state?  For example a writer monad forces collapsing of outputs from
computations into a Monoidal structure (e.g. list).

Monad and comonad are both continuations, a monad places a continuation at the
output of the previous one, a comonad places a continuation at the input of the
previous one.

More succinctly::

  comonad: (final) extract $ f <<= ... f1 <<= f2 <<= f3 ... <<= x (initial)
  monad: (initial) return x >>= f1 >>= f2 >>= f3 ... >>= f (final)

A comonad keeps adding functions in front of a closed initial state, finally
when you extract the state you will get a result after applying all these
functions to the initial state. Notice how closely it resembles to continuation
passing style. In fact we can use a CPS data type to help us convert a comonad
to a monad. A monad, on the other hand, puts a state in a one-way open world
and then allows operating on it in that world, but never allowing anything to
be retrieved from that world, the final result is after applying all the
functions.

Using comonad:

Monadic functions take pure values and result in a monadic output which can
then be composed with other monadic functions using bind. A pure value can be
converted into monadic using "return". The final result is always a monadic
value. The last thing in a monadic function is always a "return".

Once a value is inserted (returned) into a monad you cannot get it out as a
pure value.

A comonadic function always takes a comonadic value as input and results in a
pure value. The first thing in a comonadic function will be an "extract" to get
a pure value from the comonadic context and then compose it with other pure
values finally resulting in a pure value. Two comonadic functions can be
composed using "extend".

In a comonadic function, do all input args have to be comonadic or one or more?

Once you extract a value from a comonad you cannot put it back.

Examples:

IO is a monad since it is an open world state, effects are a change in the
state of that open world, we can put values in it i.e. effect a change in it
but cannot take back.

A comonad on the other hand is a closed world, you can extract values from it
but cannot put back once extracted. A "Store" comonad is more like an opaque
type enclosing some state, after the computations are done composing we can
finally extract the state.

Can we use a comonad where an existential is needed otherwise? See
https://www.schoolofhaskell.com/user/edwardk/cellular-automata/part-2

Generalising:

A structure that puts the same structure at both ends becomes less powerful.
For example Arrows (f (a,b) -> f (a -> c) -> f (b -> d) -> f (b,d)) or
Applicatives (f (a -> b) -> f a -> f b), they both have the same structure on
input and output ends. But how about something like w a -> m b? or in fact
(w a, b) -> (c, m d). Does such a thing exist?

Monad vs Comonad
----------------

A Monad can be likened to a Mealy machine and a comonad to a Moore machine. You
can always convert a comonad into a monad
(http://comonad.com/reader/2011/monads-from-comonads/) but vice-versa may not
be true. It may be easier to think in the way a Moore machine can always be
converted to a Mealy machine but vice-versa is not always true.

XXX end/coend ~ existential

Converting a comonad to a monad

::
  newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }

+-------------------------------------------------+-----------------------------------------------------+
| Monad                                           | Comonad                                             |
+=================================================+=====================================================+
| return :: a -> m a                              | extract :: w a -> a                                 |
+-------------------------------------------------+-----------------------------------------------------+
| bind :: (a -> m b) -> (m a -> m b)              | extend :: (w a -> b) -> (w a -> w b)                |
+-------------------------------------------------+-----------------------------------------------------+
| .. raw:: html                                                                                         |
|                                                                                                       |
|    <center>                                                                                           |
|                                                                                                       |
| **Laws**                                                                                              |
|                                                                                                       |
| .. raw:: html                                                                                         |
|                                                                                                       |
|    </center>                                                                                          |
+-------------------------------------------------+-----------------------------------------------------+
| bind return = id                                | extend extract = id                                 |
+-------------------------------------------------+-----------------------------------------------------+
| bind f . return = f                             | extract . extend f = f                              |
+-------------------------------------------------+-----------------------------------------------------+
| bind f . bind g = bind (bind g . f)             | extend f . extend g = extend (f . extend g)         |
+-------------------------------------------------+-----------------------------------------------------+
| .. raw:: html                                                                                         |
|                                                                                                       |
|    <center>                                                                                           |
|                                                                                                       |
| **Join and Duplicate**                                                                                |
|                                                                                                       |
| .. raw:: html                                                                                         |
|                                                                                                       |
|    </center>                                                                                          |
+-------------------------------------------------+-----------------------------------------------------+
| join :: Monad m => m (m a) -> m a               | duplicate :: Comonad w => w a -> w (w a)            |
+-------------------------------------------------+-----------------------------------------------------+
| join = bind id                                  | duplicate = extend id                               |
+-------------------------------------------------+-----------------------------------------------------+
| bind :: Monad m => (a -> m b) -> (m a -> m b)   | extend :: Comonad w => (w a -> b) -> (w a -> w b)   |
+-------------------------------------------------+-----------------------------------------------------+
| bind f = join . fmap f                          | extend f = fmap f . duplicate                       |
+-------------------------------------------------+-----------------------------------------------------+

::

  (=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> (w a -> c)

* Monad composes actions that are producers of functors (`m a` is in output
  position), comonad composes actions that are consumers of functors (`w a` is
  in input position).
* Monadic action produces positive side effects i.e. side effects are in the
  positive position. Comonadic action consumes negative side effects i.e. side
  effects are in negative position.
* Monadic action produces a container or functor layer which is then eliminated
  by a ``join``. Comonadic action consumes a container or functor layer which is
  created by ``duplicate``.
* Monad is provided an environment to run under. Comonad builds an environment?
  that is consumer of environment vs builder of state.
* A Monadic context keeps distributing state to consumers, a comonadic context
  keeps collecting produced state. On the other hand a monadic conetxt collects
  produced effects and a comonadic context produces effects to be collected by
  the actions being composed.
* In a monad the interpreter operates on the state or builds the state, the
  function can take it as input and produce some independent output. In a
  comonad the function builds the state, the interpreter passes it on to the
  function and then takes it out.
* In a monad the function does not know the structure of the state, the
  interpreter knows it and operates on it. It can provide a part of it to the
  function and then take the output of the function and merge it into the
  state.
* In a comonad we have a reverse interpreter. The functions that we are
  composing have a knowledge about the structure of the state and operate on
  it. The interpreter extracts the built copy and then provides an input and
  the accumulated state to the next builder function.
* In a Monad functions produce something and the interpreter assimilates it
  into a larger structure. In a comonad the interpreter produces something and
  the functions assimilate it into a larger structure.
* In a Monad the larger structure is opaque to the functions. Whereas in a
  comonad the larger structure is opaque to the interpreter. That's why it can
  be considered parallel to object oriented programming. The functions embed
  the knowledge of the structure.
* In a Monad the interpreter threads around state carrying functions on the
  side track. In a comonad a state carrying functions is threaded through
  the composing functions and it returns a final value. We then extend that
  returned value to convert it to the function again so that we can feed it to
  the next builder.
* A monad spits out the side effects and the context or the container collects
  and assimilates them in a data structure that it knows about. On the other
  hand in a comonad the actions suck in the side effects from the context  and
  assimilates them in a data structure that it knows about.

* In a monad the two tracks are joined at consumer end of the function i.e.
  both the inputs are provided. In a comonad we extract the other track at
  producing end of the function.
* The m or w in a monad or comonad represents a spiced up state i.e. a value
  with both the tracks, explicit and hidden. A monadic function returns an "m
  a" which means it returns two tracks. Similarly in a comonad we pass "w a"
  which means we are passing two tracks. `m a` or `w a` is a `function` carrying
  state plus explicit value i.e. an overloaded value.

* Comonad has a corecursive structure and monad has a recursive structure.
  monad is like a finite list and comonad like an infinite stream. isn't that
  why hierarchy libraries streaming implementation uses a comonad?

* finite recursive data structures are more likely to fit in a monadic
  structure whereas infinite corecursive data structures fit better in a
  comonadic structure. cellular automata, zippers are some examples of infinite
  comanadic data structures.

* In fact duplicate can be defined naturally as a corecursive data structure
  e.g. this from Dan Piponi's blog:
  >    cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)

* A monad is a linked list of functors (note finite) and a comonad is a stream
  of functors (note infinite).

When to use a monad or comonad?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can achieve the same thing using a monad or comonad but some things are more
natural to model as a monad and others as a comonad. For example an infinite
stream can be represented as an open list or a closed corecursive stream.
In asyncly we use the Context as a state being passed inside a monad.
However this can be modeled as a comonad as well. Ekmett modeled the foldl
library using comonadic folds instead.

* http://blog.sigfpe.com/2006/06/monads-kleisli-arrows-comonads-and.html
* https://www.schoolofhaskell.com/user/edwardk/cellular-automata

Composing Monads
----------------

Composing means we want to combine monads that have a different type of hidden
tracks. This is a problem similar to composing functions like f :: a -> b and g
:: c -> d.  So that we can use any of those functions anywhere in the
composition and use the return value.

Product Style   | transformers/mtl
Coproduct Style | Freer-effects

mtl is product style that's why the number of instances grow by nxn. Product is
an ad-hoc composition. Whereas sum is a patterned composition.

* https://hackage.haskell.org/package/monad-products
* https://hackage.haskell.org/package/MonadCompose

References
----------

* https://wiki.haskell.org/Typeclassopedia
* https://en.wikipedia.org/wiki/Monoidal_category
* https://monadmadness.wordpress.com/2015/01/02/monoids-functors-applicatives-and-monads-10-main-ideas/
* https://arxiv.org/pdf/1406.4823.pdf Notions of Computation as Monoids
* http://stackoverflow.com/questions/35013293/what-is-applicative-functor-definition-from-the-category-theory-pov
* http://stackoverflow.com/questions/17376038/what-exactly-are-the-categories-that-are-being-mapped-by-applicative-functors

* https://wiki.haskell.org/All_About_Monads
* https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/
* http://okmij.org/ftp/Computation/free-monad.html
* https://jaspervdj.be/posts/2012-09-07-applicative-bidirectional-serialization-combinators.html
* http://okmij.org/ftp/Haskell/zseq.pdf reflection without remorse

* http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
* http://gelisam.blogspot.in/2013/07/comonads-are-neighbourhoods-not-objects.html

* https://bartoszmilewski.com/2016/06/15/freeforgetful-adjunctions/
* https://www.schoolofhaskell.com/user/dolio/many-roads-to-free-monads

* http://www.slideshare.net/davidoverton/comonad
* https://bartoszmilewski.com/2017/01/02/comonads/

* https://en.wikipedia.org/wiki/Fundamental_theorem_of_software_engineering
* https://stackoverflow.com/questions/24112786/why-should-applicative-be-a-superclass-of-monad

* http://homepages.inf.ed.ac.uk/wadler/topics/monads.html

* https://stackoverflow.com/questions/33155331/are-and-operators-sufficient-to-make-every-possible-logical-expression
* https://en.wikipedia.org/wiki/Functional_completeness
