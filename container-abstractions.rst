Container Abstractions
======================

The common operations that you perform on containers are folds and traversals.

base | distributive | mono-traversable | lens | lens-action

Conventions
-----------

TBD - need to decide whether arrows should be is-a or inverse of that.
Higher level stuff above and lower level below?

Folds
-----

* http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire

Streaming folds:
* http://squing.blogspot.in/2008/11/beautiful-folding.html
* http://conal.net/blog/posts/another-lovely-example-of-type-class-morphisms
* http://conal.net/blog/posts/more-beautiful-fold-zipping
* http://www.haskellforall.com/2013/08/composable-streaming-folds.html
* https://www.schoolofhaskell.com/user/edwardk/cellular-automata/part-2

Category theory of folds:
* https://wiki.haskell.org/Catamorphisms
* https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms
* https://ulissesaraujo.wordpress.com/2007/12/19/catamorphisms-in-haskell/

Traversal & folds using attribute grammar:
* https://wiki.haskell.org/Attribute_grammar

* the ``foldl`` and ``folds`` packages

Basic Typeclasses
-----------------

Traversable -> Distributive  -- function application (fmap, functor) and fold (foldable) results
|                            -- unfold a value and distribute as argument for consumption
|
v
Functor, Foldable   -- values folded as pure data (does not require functor instance)

Foldable
--------

Typeclass Functions
~~~~~~~~~~~~~~~~~~~

fold:

Given a list container ``[1,2,3,4,5]``. There are two ways to compute the sum
of its elements::

  sum s (x : xs) = x + sum s xs    -- 1 + (2 + (3 + (4 + 5))) right associative
  sum s (x : xs) = sum (s + x) xs  -- (((1 + 2) + 3) + 4) + 5 left associative

The right associative version is called the right fold and the left associative
version is called the left fold. Note in foldr the fold operation is at the top
of the expression and recursion occurs as part of it. In foldl recursive call
is at the top level of the expression and the fold operation occurs as part of
it.

Now, the behavior of these operations depends on the evaluation strategy. When
the operation (+) is strict the right fold puts the whole structure on the
stack and unravels it in the end, whereas the left fold does not use the stack
at all. Note that when the container is strict we have already consumed space
in the container and the left fold does not require more space, on the other
hand when the container is lazy it is not using any space but we need that
space at the end when folding so they both are equivalent in that sense. They
are just duals of each other.

On the other hand when the operation is lazy in the second argument, the right
fold can keep yielding the next element from the container whereas the left
fold keeps building a lazy structure until the whole container is consumed,
consuming space proportional to the container size.  Note that in this example
we are using the (+) operation which is trivially strict.  However a folding
operation can be a whole pipeline of lazy operations.

To conclude, when consuming a structure (strict or lazy) using a pipeline of
lazy evaluation right fold is most efficient. For consuming a structure (strict
or lazy) using strict evaluation, left fold is most efficient. Combining a
strict operation on a large structure with a right fold or a lazy operation on
a large structure with a left fold may not be a good idea, it may not scale.
When we need to do that dividing up the structure in chunks and then folding is
a good strategy.

Lazy right fold = good - pull - infinite structures ok
Strict left fold = good - push - infinite structures ok
Strict right fold = bad - structure must be finite -- consumes the whole structure strictly, accumulating it on the stack.
Lazy left fold = bad - structure must be finite -- builds a lazy structure

Note that IO monad is strict. So to finally consume the output or input it is
inevitable to face the combination of strict evaluation and lazy structures.
However if the IO does not need to accumulate we can have a full lazy pipeline,
consuming one input and producing one output at a time (or chunks). However if
we use IO monad in them middle of a computation it cannot scale unless we use a
limited buffer.


foldr:

The following equations hold for a list::

  foldr (:) [] xs == xs
  map f = foldr ((:) . f) []

A lazy right fold can be equated with a pull style operation where the consumer
keeps pulling the next element from the container on-demand.


foldl:

Note that the ``identity`` is folded with the first element, therefore the
following reverses the list::

  reverse xs = foldl (flip (:)) []

A strict left fold is a push style mechanism where the producer keeps pushing
the next element to the consumer.

Mnemonics:

fold: remember "fuze it" - first argument is the fold function (fu), the second
is zero (ze), and ``it`` refers to the container we are folding.

fuze it : fold (fu)unction ze(ro) it

Argument order: The fold function in foldr takes the element first whereas in
foldl it takes the list first which is in accordance with their behavior of
reducing the element first or the list first.

foldl makes sense in general for left-associative operators, and foldr makes
sense for right-associative operators.  Left-associative operators must have
the type (a -> b -> a) i.e. result type is same as left argument e.g. ``(m >>=
f1) >>= f2``, while right-associative operators must have type (a -> b -> b)
i.e. result type is the same as the right argument e.g. ``1 : (2 : [])``.

+-----------------------------------------------------------------------------+
| Folds                                                                       |
+----------------------+------------------------------------------------------+
| fold a container of  | fold :: Monoid m => t m -> m                         |
| monoids              |                                                      |
+----------------------+------------------------------------------------------+
| map elements to      | foldMap :: Monoid m => (a -> m) -> t a -> m          |
| monoids then fold    |                                                      |
+----------------------+------------------------------------------------------+
| Right fold           | foldr :: (a -> b -> b) -> b -> t a -> b              |
+----------------------+------------------------------------------------------+
| Left fold            | foldl :: (b -> a -> b) -> b -> t a -> b              |
+----------------------+------------------------------------------------------+
| fold a nonempty      | foldr1/foldl1 :: (a -> a -> a) -> t a -> a           |
| container            |                                                      |
+----------------------+------------------------------------------------------+

::

  fold $ map Sum [1,2,3]
  foldMap Sum [1,2,3]

+--------+------+--------+------+---------+---------+-----+---------+
| toList | null | length | elem | maximum | minimum | sum | product |
+--------+------+--------+------+---------+---------+-----+---------+

Other Functions
~~~~~~~~~~~~~~~

+---------+-----------+-----+----+-----+-----+-----------+-----------+
| concat  | concatMap | and | or | any | all | maximumBy | minimumBy |
+---------+-----------+-----+----+-----+-----+-----------+-----------+

+---------+-----------+
| notElem | find      |
+---------+-----------+

Fold Actions
~~~~~~~~~~~~

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

Traversable and Distributive are duals of each other

+---------------------------------------------------------------------------------+
| sequence and distribute are duals of each other.                                |
+------------+----------------------------------+---------------------------------+
| sequence   | Collect the outputs of,          | ``sequence [print 1, print 2]`` |
|            | producers in the container, to   |                                 |
|            | produce a single output          |                                 |
+------------+----------------------------------+---------------------------------+
| distribute | Consume a single input and       |                                 |
|            | distribute it to the consumers   | ``distribute [(+1), (+2)] 1``   |
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

  Distributive g

  sequenceA  :: Applicative f => t (f a) -> f (t a)
  distribute :: Functor f     => f (g a) -> g (f a)

  traverse   :: Applicative f => (a -> f b) -> t a -> f (t b)
  cotraverse :: Functor f     => (f a -> b) -> f (g a) -> g b

::

  Distributive ((->) e) -- function application is distributive

  distribute [(+1), (+2)] 1
  collect id [(+1), (+2)] 1
  collect ((+1) . ) [(+1), (+2)] 1

  sequence_ $ distributeM [print, putStrLn] "5"

lens
----

* http://hackage.haskell.org/package/lens-tutorial-1.0.2

Lenses allow you to magnify and view a small part of a big structure. They
allow you to traverse or fold all or a part of any data structure not just
containers like lists but even monomorphic type data structures. Lenses can be
composed to create more sophisticated traversal or fold mechanisms.

Lens:
* Source object: s
* The part inside the source that we are focusing on: a

A lens encodes enough information so that we can generically adapt it to view,
set, over functions. A lens is just a type synonum of a function.

Lens is a function specific to source. If you provide it a way to transform a
type into a functor, it will give you a way to transform the source into the
same functor. It does not matter which functor. The functor provides another
level of abstraction for the transformation so that it can work for pure
transformations as well as side effects. For pure values we can put them
Identity functor to make this work and then take them out.

type Lens s a = Functor f => (a -> f a) -> (s -> f s)

The functor can be provided by the adapter functions like `over`.

over :: Lens s a -> (a -> a) -> (s -> s). We can read that as: Given a lens
focusing on an a inside of an s, and a function from a to a, and an s, I can
give you back a modified s from applying the function to the focus point of the
lens.

over is a generic function, you just give it a lens and corresponding value
transfomer it will provide you the source transfomer:
over: Lens s a -> Transformer a -> Transformer s

over is a higher rank function. It is like a broker or adapter fitting multiple
compatible things together.

view :: Lens s a -> s -> a. We can read this as: Given a lens that focuses on
an a inside of an s, and an s, I can give you an a.

view ln s = getConst $ ln Const s

--------

type Lens' s a = Functor f => (a -> f a) -> s -> f s
type Lens s t a b = Functor f => (a -> f b) -> s -> f t
-- you can change the type of the focus and the type of the source as well as a
result of an update.
-- Setter also has similar type except that f is Settable instead of a Functor

Notice that Lens or Setter is a generalization of a Functor:
* fmap transforms (a ->   b) -> (f a -> f b)
* Lens transforms (a -> f b) -> (  s -> f t)

* sets or setting takes a fmap like function
* The Setter mapped is merely "sets fmap"
* "over mapped" is just "fmap"

Types::

  s (contains) a
  |            | changes to
  v            v
  t (contains) b

+----------------+------------------------------------------------------------+
| instrument     | is also a                                                  |
+================+============================================================+
| iso            | lens, prism (invertible i.e. s t a b | a b s t)            |
+----------------+------------------------------------------------------------+
| prism          | (getter b t) | traversal s t a b                           |
| (dual of lens?)|                                                            |
+----------------+------------------------------------------------------------+
| lens           | getter, traversal                                          |
+----------------+------------------------------------------------------------+
| getter         | fold, action                                               |
+----------------+------------------------------------------------------------+
| traversal      | setter, fold                                               |
+----------------+------------------------------------------------------------+

* lens is a traversal AND a getter
* prism is a traversal OR a reverse getter

::

  Iso  ->  Prism -- (re) --> review (reverse getter)
  |           |
  v           v
  Lens -> Traversal -> Setter
  |           |
  v           v
  Getter -> Fold
  |           |
  v           v
  Action -> MonadicFold

Put the above diagram in a tree form.
Provide links to hackage docs.

Types::

  type Iso   s t a b = forall p f. (Profunctor p, Functor f) => p a (f b)  -> p s (f t)
  type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b)  -> p s (f t)
  type Lens  s t a b = forall f.   Functor f                 => (a -> f b) -> s -> f t

Prism examples

+------------------------------------+--------------------------------+
| ``Left "hello" & _Left %~ length`` | ``Left 5``                     |
+------------------------------------+--------------------------------+
| ``re _Left :: Contravariant f => LensLike' f a (Either a c)``       |
+------------------------------------+--------------------------------+
| ``5^.re _Left``                    | ``Left 5`` -- contravariant    |
+------------------------------------+--------------------------------+

Operators
~~~~~~~~~

* ('<&>') = 'flip' 'fmap'
* flip argument order of composite functions
* fab ?? a = fmap ($ a) fab

* Lens combinators are left associative
  (10,20) & _2  +~ 1 & _1 -~ 1
  ((((10,20) & _2)  +~ 1) & _1) -~ 1

* Lens combinators compose in the opposite direction to "."

* TODO: verify and add more operators from
  https://hackage.haskell.org/package/lens-4.15/docs/Control-Lens-Lens.html

+-----------------------------------------------------------+
| A ^ prefix implies view/fold operations                   |
+=======================================+===================+
| view (a)                              | ``^.``            |
+---------------------------------------+-------------------+
| iview ((i, a))                        | ``^@.``           |
+---------------------------------------+-------------------+
| safe retrieval (Maybe a)              | ``^?``            |
+---------------------------------------+-------------------+
| unsafe retrieval (a)                  | ``^?!``           |
+---------------------------------------+-------------------+
| toListOf ([a])                        | ``^..``           |
+---------------------------------------+-------------------+
| Actions & Monadic folds (^@ for indexed versions)         |
+---------------------------------------+-------------------+
| action                                | ``^! ^@!``        |
+---------------------------------------+-------------------+
| MonadicFold collect all results       | ``^!! ^@!!``      |
+---------------------------------------+-------------------+
| MonadicFold collect leftmost result   | ``^!? ^@!?``      |
+---------------------------------------+-------------------+

+-----------------------------------------------------------------------------+
| Set or traversal ops                                                        |
+=============================================================================+
| A ~ or = suffix implies set or traversal ops                                |
+-----------+-------------------+--------------------------+------------------+
| Suffix ~  | set pure          | ``(10,20) & _2  +~ 1``   | ``(10,21)``      |
+-----------+-------------------+--------------------------+------------------+
| Suffix =  | set state monad   | ``execState (do _2 += 1) | ``(10,21)``      |
|           |                   | (10,20)``                |                  |
+-----------+-------------------+--------------------------+------------------+
| May optionally have a prefix which is either < or <<                        |
+-----------+-------------------+--------------------------+------------------+
| Prefix <  | return the result | ``(10,20) & _2 <+~ 1``   | ``(21,(10,21))`` |
+-----------+-------------------+--------------------------+------------------+
| Prefix << | return the old    |                                             |
|           | value             |                                             |
+-----------+-------------------+---------------------------------------------+

+-------------------------------------------+
| Set or traversal operations               |
+===========================================+
| Supporting ~ = < << suffixes              |
+-------------------+-----------------------+
| set               | ``.``                 |
+-------------------+-----------------------+
| over              | ``%``                 |
+-------------------+-----------------------+
| Supporting ~ = < suffixes                 |
+-------------------+-----------------------+
| iover             | ``%@``                |
+-------------------+-----------------------+
| Math              | ``+ - * // ^ ^^ **``  |
+-------------------+-----------------------+
| Logic             | ``|| &&``             |
+-------------------+-----------------------+
| Monoid            | ``<>``                |
+-------------------+-----------------------+
| Bits              | ``.|. .&.``           |
+-------------------+-----------------------+
| FilePath          | ``</> <.>``           |
+-------------------+-----------------------+
| Supporting ~ = suffixes only              |
+-------------------+-----------------------+
| iset              | ``.@``                |
+-------------------+-----------------------+
| traverseOf        | ``%%``                |
+-------------------+-----------------------+
| Indexed traverse  | ``%%@``               |
+-------------------+-----------------------+

mono-traversable
~~~~~~~~~~~~~~~~

References
----------

* https://en.wikipedia.org/wiki/Fold_(higher-order_function)
* http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html
* http://lens.github.io/tutorial.html
* http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html
* http://blog.jakuba.net/2014/08/06/lens-tutorial-stab-traversal-part-2.html
* https://artyom.me/lens-over-tea-4

* https://groups.google.com/forum/#!topic/elm-discuss/ehsV6-YveFA fold function argument order
* http://www.cs.nott.ac.uk/~pszgmh/fold.pdf A tutorial on the universality and
  expressiveness of fold
