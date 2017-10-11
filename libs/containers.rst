Folds, Traversals and Optics
============================

The common operations that you perform on containers are folds and traversals.

base | distributive | mono-traversable | lens | lens-action

* the ``foldl`` and ``folds`` packages
* https://hackage.haskell.org/package/unfoldable

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
| view/fold operations (^ prefix)                           |
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
| Set or traversal ops (~ or = suffix, optional < or << prefix)               |
+=============================================================================+
| Suffixes ~ or = indicate set or traversal ops                               |
+-----------+-------------------+--------------------------+------------------+
| Suffix ~  | set pure          | ``(10,20) & _2  +~ 1``   | ``(10,21)``      |
+-----------+-------------------+--------------------------+------------------+
| Suffix =  | set state monad   | ``execState (do _2 += 1) | ``(10,21)``      |
|           |                   | (10,20)``                |                  |
+-----------+-------------------+--------------------------+------------------+
| < or << prefixes are used to return the new or old value respectively       |
+-----------+-------------------+--------------------------+------------------+
| Prefix <  | return the result | ``(10,20) & _2 <+~ 1``   | ``(21,(10,21))`` |
+-----------+-------------------+--------------------------+------------------+
| Prefix << | return the old    |                                             |
|           | value             |                                             |
+-----------+-------------------+---------------------------------------------+

+-------------------------------------------+
| Set or traversal operations               |
+===================+=======================+
| Opt Prefixes: < <<| Suffixes: ~ =         |
+-------------------+-----------------------+
| set               | ``.``                 |
+-------------------+-----------------------+
| over              | ``%``                 |
+-------------------+-----------------------+
+-------------------+-----------------------+
| Opt Prefix: <     | Suffixes: ~ =         |
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
+-------------------+-----------------------+
|                   | Suffixes: ~ =         |
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

* http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html
* http://lens.github.io/tutorial.html
* http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html
* http://blog.jakuba.net/2014/08/06/lens-tutorial-stab-traversal-part-2.html
* https://artyom.me/lens-over-tea-4
* https://patternsinfp.wordpress.com/2011/01/31/lenses-are-the-coalgebras-for-the-costate-comonad/
* https://arxiv.org/pdf/1103.2841.pdf Functor is to Lens as Applicative is to Biplate Introducing Multiplate
