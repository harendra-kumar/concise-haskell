The indirection allows Functors to have a hidden track during composition.
The hidden track can be used for side effects among other things.

Higher order Transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A combination of type level transformation (Functor) and value level
transformation (function application). The type level information provides a
fixed structure to data and the value level information provides the logic to
manipulate, compose or fold those structrues.

* Functor + Apply = Applicative Functors (pure, apply (<*>))
* Applicative + Monoid (sum) = Alternative (empty, choice (<|>))
* Applicative + Monoid (product) = Monad (return, bind (>>=))

* Monad

  * Monad + Alternative + Monoid (sum) = MonadPlus

  * MonadFix

* Comonad

* Category

  * Arrow

    * ArrowZero

      * ArrowZero + Monoid = ArroPlus

    * Arrow + Apply = ArrowApply

    * ArrowChoice

    * ArrowLoop

* ArrowApply <=> Monad

Monads
------

Do Expression
~~~~~~~~~~~~~

* TBD
* desugaring
* let in a do block
* where in a do block - cannot refer to bindings extracted from a monad

+-----------------------------------------------------------------------------+
| Multiline expressions in do syntax must be indented beyond the variable name|
+------------------------------------+----------------------------------------+
| Correct                            | Wrong                                  |
+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  main = do                         |  main = do                             |
|    let foo = case 0 of             |    let foo = case 0 of                 |
|         0 -> 4                     |        0 -> 4                          |
|    return ()                       |    return ()                           |
+------------------------------------+----------------------------------------+

Type Inference
--------------

* explain type inference for the programmer in monadic composition

CoAlternative
-------------

Have a "full" instead of empty.

-- Because of the type of 'from', 'Coaplicative' must correspond to "non-empty containers", for the usual hand-wavy definition of "container".
class Functor f => Coapplicative f where
  from :: f a -> a
  separate :: f (Either a b) -> Either (f a) (f b)

instance Coapplicative Identity where
  from (Identity a) = a
  separate (Identity (Left a))  = Left  (Identity a)
  separate (Identity (Right b)) = Right (Identity b)

-- Silly Haskell having no nontrivial comonoids.
class Coapplicative f => Coalternative f where
  full :: f a -> ()
  full _ = ()

  split :: f a -> f (a, a)
  split = fmap (\a -> (a,a))

Free Composition
----------------

Most of the time haskell uses binary composition which is the simplest and most
reusable thing.
However we also need real n-ary operations that make the big picture visible
and not just two elements like in the binary operations. For example to run n
tasks in parallel combining them in binary manner could be very inefficient, we
need combine them all to a single output channel to reduce communication.
Similarly we need n-ary applicatives as well.

A free construction is like a buffered processing rather than a streamed
processing. We collect and then combine rather than keep combining as we
collect. A buffered processing allows us to do interesting transformations on
the collection before combining. This is important in many cases just buffering
and batching is important in many real-world use-cases for efficiency.

A lot of arguments about free-monad and the use of other free-structures vs
using the type-classes is misplaced. free-monads are to be used to improve the
speed of the algorithm rather than the speed of the program. They are supposed
to work at a higher level rather than at a lower level for each and everything.
For example if your algorithm requires batching of requests before you can
process them then you use a free construction. This is exactly the way you use
buffering, caching or batching in an application.

Many high-performance systems applications will be just dead if there is no
buffering, it is a very important concept. And we need a composable abstraction
for it which is a free construction of an abstract composition.

Also if the buffer becomes too large you have a problem because the processing
becomes expensive and can be very bursty. The same way free monads when
constructed in an unlimited manner can be very bad for performance. Most of the
solutions to the problem sound like "optimizing the buffering data structure"
which is good but the overarching question is do we need unlimited buffers for
our problem?

References
----------

* http://stackoverflow.com/questions/18024924/haskell-why-is-a-multi-line-let-expression-a-syntax-error
* https://markkarpov.com/post/free-monad-considered-harmful.html
