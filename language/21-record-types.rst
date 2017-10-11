Record Types
============

Record Syntax
-------------

+-----------------------------------------------------------------------------+
| `-XNoTraditionalRecordSyntax` (7.4.1) -- to disable the record syntax       |
+=============================================================================+
| .. class :: center                                                          |
|                                                                             |
| Records                                                                     |
+----------------------+------------------------------------------------------+
| ::                   | ::                                                   |
|                      |                                                      |
|  data R =            |   data R where                                       |
|    R {               |     R :: {                                           |
|        x :: String   |         x  :: String                                 |
|      , y :: Int      |       , y  :: Int                                    |
|    } deriving (Show) |       } -> R                                         |
|                      |     deriving (Show)                                  |
+----------------------+------------------------------------------------------+
| Selector functions to extract a field from a record data structure are      |
| automatically generated for each record field::                             |
|                                                                             |
|  x :: R -> String                                                           |
|  y :: R -> Int                                                              |
+-----------------------------------------------------------------------------+
| Until the brain gets trained, it is pretty confusing that the types of the  |
| selector functions are different from what they seem to be from the code:   |
+-----------------------------------+-----------------------------------------+
| ::                                | ::                                      |
|                                   |                                         |
|  data R =                         |  --                                     |
|    R {                            |                                         |
|        x :: String                |  x  :: R -> String                      |
|      , y :: Int                   |  y  :: R -> Int                         |
|    }                              |                                         |
+-----------------------------------+-----------------------------------------+
| `-XDuplicateRecordFields` (8.0.1) allows using identical fields in different|
| records even in the same module. Selector functions and updates are         |
| disambiguated using the type of the field or the record.                    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data S =                                                                   |
|    S {                                                                      |
|        x :: String                                                          |
|      , z :: Int                                                             |
|    } deriving (Show)                                                        |
+-----------------------------------------------------------------------------+
| Exporting and importing selector functions:                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  Module M (y)    where ...     -- only when y is unambiguous field          |
|  Module M (R(x)) where ...     -- even when x is ambiguous field            |
|                                                                             |
|  import M (y)                  -- only when y is unambiguous field          |
|  import M (R(x))               -- even when x is ambiguous field            |
+-----------------------------------------------------------------------------+

Construction And Pattern Matching
---------------------------------

+-----------------------------------------------------------------------------+
| Construction and pattern matching                                           |
+=============================================================================+
| Record constructor brackets {} have a higher precedence than function       |
| application.                                                                |
+-----------------------------------------------------------------------------+
| `-XDisambiguateRecordFields` allows using record fields x and y unqualified |
| even if they clash with field names in other records and even when the      |
| record is defined in a module which is imported qualified.                  |
+-----------------------------------------------------------------------------+
| Note that selector functions are symbols but field names are literals i.e.  |
| you cannot say x = y and then use x in place of y as a field name. x will   |
| refer to the selector function, when used as a field name it will refer to  |
| field named "x" rather than "y".                                            |
+-----------------------------------------------------------------------------+
| **Construction**                                                            |
+----------------------------+------------------------------------------------+
| ``show (R "a" 1)``         | ``show R { y = 1, x = "a" }                    |
|                            | -- Note precedence of {}``                     |
+----------------------------+------------------------------------------------+
| ``r = R "a" 1``            | ``r = R { y = 1, x = "a" }``                   |
+----------------------------+------------------------------------------------+
| `-XRecordWildCards`        | ``let {x = "a"; y = 2} in R {..}               |
|                            | -- R {x = x, y = y}``                          |
+----------------------------+------------------------------------------------+
| **Pattern matching**                                                        |
+----------------------------+------------------------------------------------+
| ``f (R _ _)   = ...``      | ``f R {}                 = ...                 |
|                            | -- Note precedence of {}``                     |
+----------------------------+------------------------------------------------+
| ``f (R "a" 1) = ...``      | ``f R {x = "a", y = 1}   = ...``               |
+----------------------------+------------------------------------------------+
| ``f (R a b) = ...``        | ``f (R {x = a, y = b})   = a ++ show b``       |
+----------------------------+------------------------------------------------+
| `-XNamedFieldPuns`         | ``f (R {x, y})           = ...                 |
|                            | -- f (R {x = x, y = y})``                      |
|                            +------------------------------------------------+
|                            | ``f (R {x, y = b})       = ...                 |
|                            | -- f (R {x = x, y = b})``                      |
|                            +------------------------------------------------+
|                            | ``f (R {M.x, M.y})       = ... -- M is module  |
|                            | qualifier``                                    |
+----------------------------+------------------------------------------------+
| `-XRecordWildCards`        | ``f (R {..})             = ...                 |
|                            | -- f (R {x = x, y = y})``                      |
| ``..`` expands to missing  +------------------------------------------------+
| `in-scope` record fields   | ``f (R {x = "a", ..})    = ...                 |
|                            | -- f (R {x = "a", y = y})``                    |
|                            +------------------------------------------------+
|                            | ``import R(y)``                                |
|                            |                                                |
|                            | ``f (R {..})             = ...                 |
|                            | -- f (R {y = y})``                             |
+----------------------------+------------------------------------------------+

Access and Update
-----------------

+-----------------------------------------------------------------------------+
| Access and update                                                           |
+=============================================================================+
| **Accessing field 'x' using its selector function**                         |
+----------------------------------+------------------------------------------+
| ``x R {x = "a", y = 1}``         | ``x r``                                  |
+----------------------------------+------------------------------------------+
| When using `-XDuplicateRecordFields` disambiguate selectors:                |
+-----------------------------------------------------------------------------+
| By inferred or explicit type of the selector function (e.g. ``x``).         |
+-----------------------+-------------------+---------------------------------+
| ``v = x :: S -> Int`` | ``v :: S -> Int`` | ``f :: (S -> Int) -> _``        |
|                       |                   |                                 |
|                       | ``v = x``         | ``f x``                         |
+-----------------------+-------------------+---------------------------------+
| By explicit but not inferred type of the record being accessed (e.g. ``s``).|
+-----------------------+-----------------------------------------------------+
| ``ok s = x (s :: S)`` | ``bad :: S -> Int``                                 |
|                       |                                                     |
|                       | ``bad s = x s        -- Ambiguous``                 |
+-----------------------+-----------------------------------------------------+
| If only one of the conflicting selectors is imported by a module then it    |
| can be used unambiguously.                                                  |
+-----------------------------------------------------------------------------+
| **Updating one or more fields**                                             |
+----------------------------------+------------------------------------------+
| ``R {x = "a", y = 1} {x = "b"}`` | ``r { x = "b", y = 2}``                  |
+----------------------------------+------------------------------------------+
| ``..`` expands to missing        | ``f (R {x = "a", ..}) = R{x = "b", ..}`` |
| `in-scope` record fields         |                                          |
+----------------------------------+------------------------------------------+
| When using `-XDuplicateRecordFields`, disambiguate duplicate fields:        |
+-----------------------------------------------------------------------------+
| By field names:                                                             |
+-----------------------------------------------------------------------------+
| ``s {z = 5} -- field z occurs only in record type S``                       |
+-----------------------------------------------------------------------------+
| By the inferred or explicit type of the update application                  |
| (e.g. ``s {x = 5}``).                                                       |
+------------------------+-------------------+--------------------------------+
| ``v = s {x = 5} :: S`` | ``v :: S -> S``   | ``f :: S -> _``                |
|                        |                   |                                |
|                        | ``v = s {x = 5}`` | ``f (s {x = 5})``              |
+------------------------+-------------------+--------------------------------+
| By the explicit but not inferred type of the record being updated           |
| (e.g. ``s``).                                                               |
+-----------------------------+-----------------------------------------------+
| ``ok s = (s :: S) {x = 5}`` | ``bad :: S``                                  |
|                             |                                               |
|                             | ``bad s = s {x = 5} -- Ambiguous``            |
+-----------------------------+-----------------------------------------------+

Default Values
--------------

We can define default values for all fields of a record and then override them
to customize::

  data Record = Record
    { a :: Int
    , b :: String
    }
  r = def $ Record {a = 10, b = "hello"}

* data-default-class


Named function arguments
------------------------

The application of defaults to a record can be used to pass named function
arguments. We pass arguments bundled in a record instead of individually:

func (def $ Record {a = 10, b = "hello"})

Mandatory and optional arguments
--------------------------------

Using rawr we can implement mandatory and optional arguments too. We provide
defaults only for optional arguments. That way we will have to pass the other
arguments if the function signature requires it. For example::

  func :: R ("a" := Int, "b" := String) -> IO ()
  func (P (_ := a :: "a" := Int, _ := b :: "b" := String)) =
      putStrLn $ show a ++ b
  def :: ("b" := String) -> R ("a" := Int, "b" := String)
  def t = (R (#a := 0) :*: R t)
  main = func (def (#b := "hello"))

In this example the argument "a" is optional and "b" is mandatory.

Use the keyword-arg example in examples dir.

GHC Internals
-------------

* GHC.OverloadedLabels
* GHC.Records
