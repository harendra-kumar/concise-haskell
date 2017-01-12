Container Abstractions
======================

Foldable
--------

Typeclass Functions
~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Folds                                                                       |
+----------------------+------------------------------------------------------+
| fold a container of  | fold :: Monoid m => t m -> m                         |
| monoids              |                                                      |
+----------------------+------------------------------------------------------+
| map elements to      | foldMap :: Monoid m => (a -> m) -> t a -> m          |
| monoid & fold        |                                                      |
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
| sequence   | Collect the outputs of producers | ``sequence [print 1, print 2]`` |
|            | in a container to produce a      |                                 |
|            | single output                    |                                 |
+------------+----------------------------------+---------------------------------+
| distribute | Consume a single input and       |                                 |
|            | distribute it to consumers in a  | ``distribute [(+1), (+2)] 1``   |
|            | container                        |                                 |
+------------+----------------------------------+---------------------------------+

+-----------------------------------------------------------------------------------+
| traverse and cotraverse are duals of each other.                                  |
+------------+----------------------------------+-----------------------------------+
| traverse   | maps a function to the members   |                                   |
|            | of container before sequence     | ``traverse print [1,2]``          |
+------------+----------------------------------+-----------------------------------+
| cotraverse | applies a function to the        |                                   |
|            | container after distribute       | ``cotraverse sum [(+1), (+2)] 1`` |
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
