Applicative Actions
===================

* Applicatives (Sequential application cf. currying or just function call)
  there is no way to compose two applicatives like function composition.
  They are just like n-ary functions. They are like a simple composed
  expressions in the pure world.

  Unlike monads, we cannot transfer context from one applicative to another.
  Picture. The context is limited to just one application and not across
  applications.

* (a -> f b) is a transition from a pure world to an abstracted world or a
  world where we can have some hidden context, two tracks and therefore can
  represent side-effects. Therefore (a -> f b) is usually an action. Though it
  only means that it can return two parallel outputs at the same time, and
  compose them in two different ways.  Similarly (f a -> b) can accept two
  parallel inputs at the same time.

* TBD - define actions, effects, effectful computation etc.
* Three types of values, abstract value (with holes), pure concrete value
  (neutral), effects (produce a side effect).

* A function inside a functor can be thought of returning two results, one pure
  and the other hidden or functorial (effect).

Monad lifts "value to functor transition functions (i.e. a -> m b) as values"
therefore it creates a bridge between an unabstracted (pure) value and an
abstracted (functor) value. There are two functors here? One the m in (a -> m
b) and the other the monad collapsing logic? If we make m as identity and keep
the other layer as it is we get an applicative. We usually implement both as
one layer, a free monad separates the two?

Applicative vs Representable
----------------------------

* Representable is a functor equivalent of a function whereas applicative is
  equivalent of a value or arguments.
* A representable can be applied to a free functor (traversable). Combines two
  functors. It is like a function application at the functor level. It combines
  two functors of different types.
* Applicative does not apply functors instead it combines the values inside
  functor. That is a function inside a functor is applied to arguments inside a
  functor. This is function application within the functor and not at the
  functor level. At the functor level this is just a custom way to combine two
  functor values of the same type.
* A representable functor is always an applicative and a monad.

Composition of Functions
------------------------

Keep in mind that applicatives, monads and arrow types compose actions or
functions.

::

  -- function
  f :: a -> b

  -- composition
  (>>>) :: (a -> b) -> (b -> c) -> (a -> c)

  -- map a function
  fmap :: (a -> b) -> (f a -> f b)

  -- to be able to use the applicative style composition we need to be able to
  -- inject values inside an applicative functor
  pure :: a -> f a

  -- apply a function inside a functor
  <*> :: f (a -> b) -> (f a -> f b)

  show ((+ 1) 5)
  Identity show <*> (Identity (+ 1) <*> Identity 5)

  -- to be able to use the monadic style composition we need to be able to
  -- inject values inside a monadic functor
  return :: a -> f a

  -- compose functions inside a monadic functor
  -- Kleisli composition
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

  show . (+1) $ 5
  return . show <=< return . (+1) $ 5 :: Identity String

  -- Applicative functor is a special case of monadic functor
  ap :: (Monad m) => m (a -> b) -> m a -> m b
  ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

  =<< :: (a -> f b) -> (f a -> f b)
  >>= :: f a -> (a -> f b) -> f b

  return :: a -> f a
  fmap   :: (a -> b)   -> (f a -> f b)
  <*>    :: f (a -> b) -> (f a -> f b)
  =<<    :: (a -> f b) -> (f a -> f b)

Applicative
-----------

Applicative functors are functors for which there is also a natural
transformation that preserve monoidal structure of their source/target
categories. In the case of Haskell's Applicative endofunctors (because their
source and target categories is Hask), the monoidal structure is the Cartesian
product. So for an Applicative functor there are natural transformations φ: (f
a, f b) -> f (a, b) and ι: () -> f ()

Also called "strong lax monoidal functor". The monoidal formulation is
more elegant. Apply a function (functor property) and combine (monoidal
property)::

  class (Functor f) => Applicative f where
    pure :: a -> f a
    zip :: (f a, f b) -> f (a, b)

A functor type allows you to have function objects wrapped in that type,
but it does not know how to apply them to values wrapped in the same
type. Applicative adds that via <*>. An applicative type provides a type
specific way of applying functions contained in that type to values
contained in that same type::

  <*> :: f (a -> b) -> f a -> f b

This is another way of composing analogous to function application.

Applicatives are more rigid and structured compared to Monads. Monads are much
more flexible as there is no enforcement on the structure. Applicatives enforce
a structure on the computation determined by the structure of the function
application. However, applicatives are more composable than Monads.
Applicatives can be freely composed to create new applicatives whereas monads
cannot be. The composition of applicative functors is always applicative,
but the composition of monads is not always a monad.

Applicative functors are a generalisation of monads. Both allow the expression
of effectful computations into an otherwise pure language, like Haskell.
Applicative functors are to be preferred to monads when the structure of a
computation is fixed a priori. That makes it possible to perform certain kinds
of static analysis on applicative values.

* https://arxiv.org/pdf/1403.0749.pdf

Examples
~~~~~~~~

List: apply a collection of functions on a collection of values and
combine the results. Its own unique way of application - apply each
function to each value and then concatenate the results::

  >> [id,id,id] <*> [1,2,3]
  [1,2,3,1,2,3,1,2,3]

IO: Apply the function to the values resulting from the IO action. Note
the function itself is NOT an IO action or something resulting from an
IO action::

  sz <- (++) <$> getLine <*> getLine

Maybe:

* https://stackoverflow.com/questions/24668313/arrows-are-exactly-equivalent-to-applicative-functors
For the difference between Applicative, monadic and arrowized IO

Conclusion

Monads are opaque to static analysis, and applicative functors are poor at
expressing dynamic-time data dependencies. It turns out arrows can provide a
sweet spot between the two: by choosing the purely functional and the arrowized
inputs carefully, it is possible to create an interface that allows for just
the right interplay of dynamic behaviour and amenability to static analysis.

* Applicative corresponds to product types or product operation or functions.
  A function or applicative requires all of the arguments to complete the
  operation while an Alternative may require only some or any of them (choice).

do desugar
~~~~~~~~~~

The do notation allows a special form of binding via the ``<-`` symbol. ``<-``
is like a ``=`` in a pattern matching equation except that the binding produced
by ``<-`` must be used in a future computation or action in the same do block
via a bind operator.

+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  do pat <- computation             |  let f pat = more                      |
|     more                           |      f _ = fail "..."                  |
|                                    |  in  computation >>= f                 |
+------------------------------------+----------------------------------------+

+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  do x1 <- action1                  |  action1 >>= (\ x1 ->                  |
|     x2 <- action2                  |    action2 >>= (\ x2 ->                |
|        action3 x1 x2               |        action3 x1 x2))                 |
+------------------------------------+----------------------------------------+

As a special case::

  do
    x1
    x2
    x3
    ...
  is x1 >> x2 >> x3 ...

You can use ``<-`` just like a ``=`` on any expression. For example::

  v <- case x of
        ...

  v <- do
        x1
        x2
        ...

* Each non-let statement in a do statement is bound by the monadic semantics

  * for example in IO monad they are evaluated sequentially
* Each variable bound by "<-" must be chained to another monadic action
* bindings produced by ``<-`` can be used in subsequent let statements in the
  same do block but cannot be used in the where block.

Evaluation semantics
^^^^^^^^^^^^^^^^^^^^

Note that when the monad is strict, each line in the do statement is evaluated
before the next line. However, any let statement evaluation is driven by the
monadic statements where they are used?

For example in the IO monad, action1 is strictly evaluated before action2
irrespective of where x1 or x2 are used in the following code::

  do x1 <- action1
     x2 <- action2
        action3 x1 x2

This is much more clearer from the desugared form of the do statement. Every
`>>=` in the desugared version is an evaluation fence. We go left to right and
anything before a fence is evaluated before anything that comes after it.

Scoping rules
^^^^^^^^^^^^^

Scoping rules for monadic variables. They are not visible in where statements,
but they are visible in the following let statements.

Applicative do
~~~~~~~~~~~~~~

TBD

Standard Applicatives
---------------------

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

Free Applicative
~~~~~~~~~~~~~~~~

Applicative functors [6] are a generalisation of monads. Both allow the
expression of effectful computations into an otherwise pure language, like
Haskell [5]. Applicative functors are to be preferred to monads when the
structure of a computation is fixed a priori. That makes it possible to perform
certain kinds of static analysis on applicative values. We define a notion of
free applicative functor, prove that it satisfies the appropriate laws, and
that the construction is left adjoint to a suitable forgetful functor. We show
how free applicative functors can be used to implement embedded DSLs which can
be statically analysed.

Free monads in Haskell are a very well-known and practically used construction.
Given any endofunctor f, the free monad on f is given by a simple inductive
definition::

  data Free f a
  = Return a
  | Free (f (Free f a))

The typical use case for this construction is creating embedded DSLs (see for
example [10], where Free is called Term). In this context, the functor f is
usually obtained as the coproduct of a number of functors representing “basic
operations”, and the resulting DSL is the minimal embedded language including
those operations.

One problem of the free monad approach is that programs written in a monadic
DSL are not amenable to static analysis. It is impossible to examine the
structure of a monadic computation without executing it.  In this paper, we
show how a similar “free construction” can be realised in the context of
applicative functors.

A free applicative requires a list type representation and therefore the most
efficient way to represent it is perhaps using difference lists as they are the
most efficient representation of lists.

* https://arxiv.org/pdf/1403.0749.pdf Free Applicative Functors
* https://www.eyrie.org/~zednenem/2013/05/27/freeapp
* https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html

Alternative
~~~~~~~~~~~

A monoid on applicative functors. A monoid means we have a way to represent a
zero or identity which means we can perform an action zero or more times and
fold the results into a list combining them in a typeclass instance specific
manner.

The basic intuition is that empty represents some sort of "failure", and (<|>)
represents a choice between alternatives.

Combines applicative actions in the following ways:

+---------------------------+-------------------------------------------------+
| empty :: f a              | Identity of the monoid                          |
+---------------------------+-------------------------------------------------+
| <\|> :: f a -> f a -> f a | In a sequence of actions composed using '<|>',  |
|                           | keep performing actions until you get a         |
|                           | result that is not ``empty``.                   |
+---------------------------+-------------------------------------------------+
| some :: f a -> f [a]      | perform an action multiple times, returns a     |
|                           | non-empty list of results or ``empty``.         |
|                           | failure, ...              = failure             |
|                           | success, failure          = success [res]       |
|                           | success, success, failure = sucess [res1, res2] |
+---------------------------+-------------------------------------------------+
| many :: f a -> f [a]      | perform an action multiple times, return an     |
|                           | empty list, a list of values.                   |
|                           | failure, ...              = []                  |
|                           | success, failure          = success [res]       |
|                           | success, success, failure = sucess [res1, res2] |
+---------------------------+-------------------------------------------------+

The intuition is that both `some` and `many` keep running `v`, collecting its
results into a list, until it fails; `some v` requires `v` to succeed at least
once, whereas `many v` does not require it to succeed at all. That is, many
represents 0 or more repetitions of `v`, whereas some represents 1 or more
repetitions.

Example: Maybe

+------+----------------------------------------------------------------------+
| <\|> | Perform an action until you get a Just value                         |
+------+----------------------------------------------------------------------+
| some | keep performing until you get a Nothing                              |
+------+----------------------------------------------------------------------+
| many | keep performing until you get a Nothing                              |
+------+----------------------------------------------------------------------+

+--------------+--------------------------------------------------------------+
| some Nothing | Nothing                                                      |
+--------------+--------------------------------------------------------------+
| many Nothing | Nothing                                                      |
+--------------+--------------------------------------------------------------+
| some Just 5  | loops forever -- because it keeps succeeding every time      |
+--------------+--------------------------------------------------------------+
| many Just 5  | loops forever -- because it keeps succeeding every time      |
+--------------+--------------------------------------------------------------+

The problem is that since `Just a` is always "successful", the recursion will
never terminate. In theory the result "should be" the infinite list [a,a,a,...]
but it cannot even start producing any elements of this list, because there is
no way for the (<*>) operator to yield any output until it knows that the
result of the call to many will be Just.

In the end, some and many really only make sense when used with some sort of
"stateful" Applicative instance, for which an action v, when run multiple
times, can succeed some finite number of times and then fail. For example,
parsers have this behavior, and indeed, parsers were the original motivating
example for the some and many methods;

Concurrently from the async package has an Alternative instance, for which c1
<|> c2 races c1 and c2 in parallel, and returns the result of whichever
finishes first. empty corresponds to the action that runs forever without
returning a value.

Practically any parser type (e.g. from parsec, megaparsec, trifecta, ...) has
an Alternative instance, where empty is an unconditional parse failure, and
(<|>) is left-biased choice. That is, p1 <|> p2 first tries parsing with p1,
and if p1 fails then it tries p2 instead.

some and many work particularly well with parser types having an Applicative
instance: if p is a parser, then some p parses one or more consecutive
occurrences of p (i.e. it will parse as many occurrences of p as possible and
then stop), and many p parses zero or more occurrences.

* http://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to

* An Alternative corresponds to Sum types the way an Applicative corresponds to
  product types.

Covariant vs Contravariant
--------------------------

  * Covariant - producers ( f a --> producer of a)

    * Applicative - combines producers covariants

      * Alternative - Monoid on applicatives

  * Contravariant - consumers ( f a --> consumer of a)

    * Divisible - combines consumers or contravariants

      * Decidable - Monoid on divisibles

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

Folding the outputs and Unfolding the inputs to Actions
-------------------------------------------------------

Traversable -> Distributive  -- function application (fmap, functor) and fold (foldable) results
|                            -- unfold a value and distribute as argument for consumption
|
v
Foldable Functor

+--------------------------------------------------------------------+
| Fold actions - ignore results                                      |
+--------------------+---------------------+-------------------------+
|                    | Applicative         | Monadic                 |
+--------------------+---------------------+-------------------------+
| Map & evaluate     | ``traverse_/for_``  | ``mapM_/forM_``         |
+--------------------+---------------------+-------------------------+
| Evaluate           |  ``sequenceA_``     | ``sequence_``           |
+--------------------+---------------------+-------------------------+
| Sum                | ``asum``            | ``msum``                |
+--------------------+---------------------+-------------------------+

Traversable & Distributive
--------------------------

Traversable and Distributive are duals of each other. Traversable collects all
outputs from producers into one and Distributive distributes one input to all
consumers. Distributive is isomorphic to the reader monad. Is traversable
isomorphic to the writer monad?

+-------------+--------------+---------+
| Pure        | Applicative  | Monad   |
+=============+==============+=========+
| Monoid      | Traversable  | Writer  |
+-------------+--------------+---------+
| Comonoid    | Distributive | Reader  |
+-------------+--------------+---------+

A writer monad accumulates outputs from individual actions into a container. In
general, each output could be accumulated as an element in the output data
structure e.g. a list. So all the outputs get "tabulated" into the target data
structure.

Similarly, in general the inputs in a reader monad can be thought of as a
"tabulated" or "accumulated" series and can be distributed to consumers based
on the index in the table.

+---------------------------------------------------------------------------------+
| sequence and distribute are duals of each other.                                |
+------------+----------------------------------+---------------------------------+
| sequence   | Collect the outputs of,          | ``sequence [print 1, print 2]`` |
|            | producers in the container, to   |                                 |
|            | produce a single output          |                                 |
+------------+----------------------------------+---------------------------------+
| distribute | Consume a single input and       |                                 |
|            | distribute it to all consumers   | ``distribute [(+1), (+2)] 1``   |
|            | in the container                 |                                 |
+------------+----------------------------------+---------------------------------+

+-----------------------------------------------------------------------------------+
| traverse and cotraverse are duals of each other.                                  |
+------------+----------------------------------+-----------------------------------+
| traverse   | maps a function over the members |                                   |
|            | of container before `sequence`   | ``traverse print [1,2]``          |
+------------+----------------------------------+-----------------------------------+
| cotraverse | applies a function to the        |                                   |
|            | container after `distribute`     | ``cotraverse sum [(+1), (+2)] 1`` |
+------------+----------------------------------+-----------------------------------+

Traversable
-----------

+--------------------------------------------------------+
| Traversable (Functor, Foldable) - Collect the outputs  |
| of producers in a container.                           |
+-------------------+------------------------------------+
| Applicative       | Monadic                            |
+-------------------+------------------------------------+
|  ``traverse/for`` | ``mapM/forM``                      |
|                   |                                    |
+-------------------+------------------------------------+
|  ``sequenceA``    | ``sequence``                       |
+-------------------+------------------------------------+

Distributive
------------

To be distributable a container will need to have a way to consistently zip a
potentially infinite number of copies of itself. This effectively means that
the holes in all values of that type, must have the same cardinality, fixed
sized vectors, infinite streams, functions, etc. and no extra information to
try to merge together.

+-----------------------------------------------------------------------------+
| Distributive (Functor) - Distribute input to consumers in a container.      |
+----------------------------------------+------------------------------------+
| Functor                                | Monadic                            |
+----------------------------------------+------------------------------------+
|                                        | ``collectM``                       |
| ``collect f = distribute . fmap f``    |                                    |
+----------------------------------------+------------------------------------+
| ``cotraverse f = fmap f . distribute`` | ``comapM``                         |
|                                        |                                    |
+----------------------------------------+------------------------------------+
| ``distribute``                         | ``distributeM``                    |
|                                        |                                    |
+----------------------------------------+------------------------------------+

::


  sequenceA  :: Applicative f => t (f a) -> f (t a)

  Distributive f
  distribute :: Functor t     => t (f a) -> f (t a)

  traverse   :: Applicative f => (a -> f b) -> t a -> f (t b)

  Distributive f
  cotraverse :: Functor t     => (t a -> b) -> t (f a) -> f b

::

  Distributive ((->) e) -- function application is distributive
  -- Looking at it in another way, it distributes the argument to each function application
  -- in the container.

  -- A list of functions turns into a function that produces a list, the
  -- function applies its argument to each element of the list.
  distribute [(+1), (*2)] 1 -- [2, 2]
  collect id [(+1), (*2)] 1
  collect ((+1) . ) [(+1), (*2)] 1

  -- A list of functions turns into a function producing a list of IO ()
  sequence_ $ distributeM [print, putStrLn] "5"

With Distributive, you can, for example, zip two containers by distributing
the Pair Functor::

  data Pair a = Pair a a deriving Functor

  zipDistributive :: Distributive f => f a -> f a -> f (a, a)
  zipDistributive xs ys = fmap f $ distribute (Pair xs ys)
    where f (Pair x y) = (x, y)

Quotes
------

Note that many such Applicatives are actually a whole family of Monads, namely
one for each "shape" of structure possible. ZipList isn't a Monad, but ZipLists
of a fixed length are. Reader is a convenient special (or is it general?) case
where the size of the "structure" is fixed as the cardinality of the
environment type.

All those zippy applicatives (whether they truncate or pad) restrict to monads
if you fix the shape in a way that amounts to a Reader monad up to isomorphism.
Once you fix the shape of a container, it effectively encodes a function from
positions, like a memo trie. Peter Hancock calls such functors "Naperian", as
they obey laws of logarithms.

References
----------

* http://comonad.com/reader/2013/algebras-of-applicatives/
* http://comonad.com/reader/2012/abstracting-with-applicatives/
* https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Data-Functor-Reverse.html foldable and traversable reversed
* Control.Applicative.Backwards
* Control.Applicative.Lift
* https://bartoszmilewski.com/2015/07/29/representable-functors/
* https://hackage.haskell.org/package/naperian Efficient representable functors
* https://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont/7072206
