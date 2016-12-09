Syntax in a Nutshell
====================

Terminology
-----------

+----------+------------------------------------------------------------------+
| REPL     | Read Eval Print Loop - an interactive language interpreter       |
+----------+------------------------------------------------------------------+
| GHC      | The glorious Glasgow Haskell Compiler                            |
+----------+------------------------------------------------------------------+
| GHCi     | The interactive REPL version of GHC                              |
+----------+------------------------------------------------------------------+
| built-in | Functionality provided by the language i.e. GHC, the             |
|          | compiler                                                         |
+----------+------------------------------------------------------------------+
| packages | Modules (libraries) live in packages which can be installed      |
|          | independently. Some packages (e.g. base) are installed with the  |
|          | compiler.                                                        |
+----------+------------------------------------------------------------------+
| base     | `base` is a package providing basic and essential functionality  |
+----------+------------------------------------------------------------------+
| Prelude  | A module from `base` package providing the bare necessities and  |
|          | imported implicitly.                                             |
+----------+------------------------------------------------------------------+

Expressions
~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| The basic building block of a Haskell program is an expression. The         |
| simplest expression is a literal data value:                                |
+-----+----+------------------------------------------------------------------+
| 'a' | 10 | 10.5                                                             |
+-----+----+------------------------------------------------------------------+
| Reducing or computing the expression to its final result is called          |
| `reduction` or `evaluation` of the expression.                              |
+-----------------------------------------------------------------------------+
| You can type these expressions in GHCi to evaluate and print the result.    |
+-----------------------------------------------------------------------------+
| A function call is an expression:                                           |
+-----------+---------+-------------------------------------------------------+
| print 'a' | even 10 | subtract 0.5 10.5                                     |
+-----------+---------+-------------------------------------------------------+
| These functions are called `prefix functions` since the function name comes |
| before its arguments.                                                       |
+-----------------------------------------------------------------------------+
| We can also use operators in an expression. A `binary operator` is an       |
| `infix function` as the function name is placed in the middle of its two    |
| arguments.                                                                  |
+------------+-------+-------+------------------------------------------------+
| 'a' == 'b' | 5 + 4 | 5 - 4 | 2^3                                            |
+------------+-------+-------+------------------------------------------------+
| A Haskell expression is composed using functions, operators and values.     |
| Arguments of functions could be expressions themselves, argument            |
| expressions must be enclosed in parenthesis.                                |
+---------------+-------------------------------------------------------------+
| 3 * 3 + 4 * 4 | print (subtract (2^3) ((5 + 4) + (5 - 4)))                  |
+---------------+-------------------------------------------------------------+

Basic Data Types (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~

+----------+------------------------------------------------------------------+
| Type     | Values                                                           |
+==========+==========+========+============+=================================+
| Char     | 'a'      | 'b'    | 'c'        | Unicode character literals      |
+----------+----------+--------+------------+---------------------------------+
| Int      | -1       | 0      | 1          | Signed, Min: 2^63, Max: 2^63 - 1|
+----------+----------+--------+------------+---------------------------------+
| Word     | 0        | 1      | 2          | Unsigned, Min: 0, Max 2^64 - 1  |
+----------+----------+--------+------------+---------------------------------+
| Float    | -5.3     | 0.33333334          | Floating point                  |
+----------+----------+---------------------+---------------------------------+
| Double   | -5.3     | 0.3333333333333333  | Double precision floating point |
+----------+----------+---------------------+---------------------------------+

* TBD string literals
* escape codes
* other ways to write literals e.g. 1.0e7

Basic Prelude Functions
-----------------------

Arithmetic
~~~~~~~~~~

* Defined in base
* TODO: point to prelude itself
* TODO: show the result of the expression

+-----------+-------------+-------------------------+
| Operation | Example     | Description             |
+===========+=============+=========================+
| \+        | 3 + 2       | Addition                |
+-----------+-------------+-------------------------+
| \-        | 3 - 2       | Subtraction             |
+-----------+-------------+-------------------------+
| \*        | 3 * 2       | Multiplication          |
+-----------+-------------+-------------------------+
| /         | 3 / 2       | Fractional division     |
+-----------+-------------+-------------------------+

+--------+----------------+---------------------------------------------------+
| ^      | 3 ^ 2          | Positive integer power                            |
+--------+----------------+---------------------------------------------------+
| ^^     | 3 ^^ 2         | Integer power                                     |
+--------+----------------+---------------------------------------------------+
| \**    | 3 \** 2.2      | Floating power                                    |
+--------+----------------+---------------------------------------------------+
| div    | 3 \`div\` (-2) | Integral division truncated towards negative      |
|        |                | infinity                                          |
+--------+----------------+---------------------------------------------------+
| mod    | 3 \`mod\` (-2) | modulus of `div`                                  |
+--------+----------------+---------------------------------------------------+
| quot   | 3 \`div\` (-2) | Integral division quotient truncated towards zero |
+--------+----------------+---------------------------------------------------+
| rem    | 3 \`div\` (-2) | remainder of `quot`                               |
+--------+----------------+---------------------------------------------------+

Comparisons
~~~~~~~~~~~

+-----------+-------------+-------------------------+
| ==        | 3 == 2      |  Equals                 |
+-----------+-------------+-------------------------+
| /=        | 3 /= 2      |  Not equal              |
+-----------+-------------+-------------------------+
| >         | 3 >  2      |  Greater than           |
+-----------+-------------+-------------------------+
| >=        | 3 >= 2      |  Greater than or equal  |
+-----------+-------------+-------------------------+
| <         | 3 <  2      |  Less than              |
+-----------+-------------+-------------------------+
| <=        | 3 <= 2      |  Less than or equal     |
+-----------+-------------+-------------------------+

Boolean Logic
~~~~~~~~~~~~~

+-----------+---------------+-------------------------+
| Operation | Example       | Remarks                 |
+===========+===============+=========================+
| ==        | True == False |                         |
+-----------+---------------+-------------------------+
| /=        | True /= False |                         |
+-----------+---------------+-------------------------+
| ||        | True || False |                         |
+-----------+---------------+-------------------------+
| &&        | True && False |                         |
+-----------+---------------+-------------------------+
| not       | not True      |                         |
+-----------+---------------+-------------------------+

Case Expressions
----------------

Case is the root source of all branching, pattern matching and strict
evaluation in Haskell. All other pattern matches and conditionals are syntactic
sugar on top of case.

tool to build ad-hoc functions.

A case expression is one of the most fundamental building blocks of Haskell.
It examines the input and allows us to evaluate a different expression in
different cases of input.

It is essentially a function which enumerates the output in different cases of
inputs making it the lowest level tool to build functions by mapping input to
ouput.

Thinking in terms of conditionals, `case` is the fundamental tool to express
branching in Haskell.

Case combined with other expressions allows us to create more complex
expressions involving branching or custom mapping to outputs based on
inputs.

Filenames
---------

+-----------+------------------+
| Extension | Meaning          |
+-----------+------------------+
| .hs       | Haskell          |
+-----------+------------------+
| .lhs      | Literate Haskell |
+-----------+------------------+

Importing Modules
-----------------

+---------------------------------------------------------------------------------------+
| Assume you want to import the function ``take`` from module ``Data.List``             |
+---------------------------------+--------------------------------+--------------------+
| import directive                | Description                    | Using ``take``     |
+=================================+================================+====================+
| import Data.List                | imports everything             | ``take``           |
+---------------------------------+--------------------------------+--------------------+
| import Data.List (take)         | import only ``take``           | ``take``           |
+---------------------------------+--------------------------------+--------------------+
| import qualified Data.List      | All qualified by ``Data.List`` | ``Data.List.take`` |
+---------------------------------+--------------------------------+--------------------+
| import qualified Data.List as L | All qualified by ``L``         | ``L.take``         |
+---------------------------------+--------------------------------+--------------------+

Operators
---------

+-----------------------------------------------------------------------------+
| Operators are just ordinary functions with a default infix syntax.          |
| The only additional property of an operator is its fixity.                  |
| TODO: What makes a valid operator identifier?                               |
+-----------------------------------------------------------------------------+

+---------------+-------------------------------------------------------------+
| Precedence    | Higher precedence operator is evaluated before lower.       |
+---------------+-------------------------------------------------------------+
| Associativity | How operators of the same precedence are grouped in the     |
|               | absence of parentheses.                                     |
+---------------+-------------------------------------------------------------+
| Fixity        | Precedence and associativity together is called fixity      |
+---------------+--------------+--------------+-------------------------------+
| Associative   | (1 + 2) + 3  | 1 + 2 + 3    | 1 + (2 + 3)                   |
+---------------+--------------+--------------+-------------------------------+
| Right         |              | 1 : 2 : []   | 1 : (2 : [])                  |
| Associative   |              |              |                               |
+---------------+--------------+--------------+-------------------------------+
| Left          | ((f x) y) z  | f x y z      |                               |
| Associative   |              |              |                               |
+---------------+--------------+--------------+-------------------------------+

Defining Operator Fixity (Precedence and Associativity)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------+---------------------------------------------------------+
| Default fixity    | Left associative, precedence 9                          |
+-------------------+---------------------------------------------------------+
| Associative       | infix `precedence` `op`                                 |
+-------------------+---------------------------------------------------------+
| Left associative  | infixl `precedence` `op`                                |
+-------------------+---------------------------------------------------------+
| Right associative | infixr `precedence` `op`                                |
+-------------------+---------------------------------------------------------+
| Precedence is an integer ranging from 0-9.                                  |
+-----------------------------------------------------------------------------+
| Numerically higher precedence operators are evaluated before lower.         |
+-----------------------------------------------------------------------------+
| Operators at the same precedence cannot be used in a single                 |
| expression without using explicit parenthesis.                              |
+-----------------------------------------------------------------------------+
| There are only two built-in operators i.e. a record creation or update      |
| (``{}``) and function application (whitespace or juxtaposition).            |
+-----------------------------------------------------------------------------+

Fixity of common operators
~~~~~~~~~~~~~~~~~~~~~~~~~~

+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Groups              | Prec| Op         | Description                              | Left Associative    | Associativity | Right Associative   |
|                     |     |            |                                          |                     | Reason        |                     |
+=====================+=====+============+==========================================+=====================+===============+=====================+
| Functionish (       |     | {}         | Record application (built-in)            | ({...} {...}) {...} |               |                     |
| application, index) +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     |            | Function application (built-in)          | (f x) y             |               |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 9   | .          | Function composition                     |                     | Reduction     | f . (g . h)         |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | !!         | List index                               | (a !! 2) !! 3       |               |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | !          | Map, Array index                         | (a ! 2) ! 3         |               |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ``\\``     | Map subtract                             | ``(a \\ b) \\ c``   | ?             |                     |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | //         | Array append                             | (a // b) // c       | ?             |                     |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Arithmetic (        | 7   | / *        | Multiplication and division              | (1 / 2) / 2         | Rounding      |                     |
| Numeric, list)      +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 6   | \+ -       | Addition and subtraction                 | (1 + 2) + 2         | Overflow      |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 5   | :          | List construction                        |                     |               | 1 : (2 : [])        |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ++         | List append                              |                     | Reduction     | a ++ (b ++ c)       |
|                     |     +------------+------------------------------------------+---------------------+---------------+---------------------+
|                     |     | ``\\``     | List subtract                            |                                                           |
+---------------------+-----+------------+------------------------------------------+-----------------------------------------------------------+
| Comparisons &       | 4   | == /=      | Comparisons and predicates               |                                                           |
| Boolean             |     | < <= > >=  |                                          |                                                           |
|                     |     | elem       |                                          |                                                           |
|                     |     | notElem    |                                          |                                                           |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 3   | &&         | boolean `and`                            |                     | Reduction     | a && (b && c)       |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 2   | ||         | boolean `or`                             |                     | Reduction     | a || (b || c)       |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Sequencing & $      | 1   | >> >>=     | Sequencing                               | (a >> b) >> c       |               |                     |
|                     +-----+------------+------------------------------------------+---------------------+---------------+---------------------+
|                     | 0   | $          | function application                     |                     |               | f $ (g $ h x)       |
+---------------------+-----+------------+------------------------------------------+---------------------+---------------+---------------------+
| Note that only ``:`` and ``$`` are right associative due to inherent semantics, the rest are right associative                                |
| only to force the reduction order of the expression for performance reasons or to force evaluation semantics.                                 |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
| Note also that all left associative operations are left associative because of inherent semantics.                                            |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
| For any other operators not in this table use hoogle to see the fixity in documentation or code.                                              |
+-----------------------------------------------------------------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Some Precedence Examples                                                    |
+==================================+==========================================+
| show R {x = 1, y = 1}            | show (R {x = 1, y = 1})                  |
+----------------------------------+------------------------------------------+
| f . g x                          | f . (g x)                                |
+----------------------------------+------------------------------------------+
| 1 * 2 + 3 + 4 / 5                | (1 * 2) + 3 + (4 / 5)                    |
+----------------------------------+------------------------------------------+
| 1 + 2 : 3 : []                   | (1 + 2) : 3 : []                         |
+----------------------------------+------------------------------------------+
| 1 == 1 && 2 > 1                  | (1 == 1) && (2 > 1)                      |
+----------------------------------+------------------------------------------+
| False && True || True            | (False && True) || True                  |
+----------------------------------+------------------------------------------+
| ``"a" ++ "b" \\ "a"``            | Cannot mix different operators with      |
|                                  | same precedence                          |
+----------------------------------+------------------------------------------+

Operators as Functions and Vice Versa
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------+---------------+
| Prefix      | Infix         |
+=============+===============+
| ``div 4 3`` | ``4 `div` 3`` |
+-------------+---------------+
| ``(+) 4 3`` | ``4 + 3``     |
+-------------+---------------+

+---------------------------------------------+
| Sections                                    |
+=============+===============================+
| ``(5 /) x`` | ``5 / x``                     |
+-------------+-------------------------------+
| ``(/ 5) x`` | ``x / 5``                     |
+-------------+-------------------------------+
| ``(5 -) x`` | ``5 - x``                     |
+-------------+-------------------------------+
| Special case: prefix ``-`` is always unary  |
+-------------+-------------------------------+
| ``(- 5)``   | ``-5``                        |
+-------------+-------------------------------+

Type Level Syntax
-----------------

Type Signatures
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A type signature can be associated with an identifer or an expression using |
| the ``::`` operator which can be read as `has type`.                        |
+----------------+------------------------------------------------------------+
| Type signature | ``<identifier or expression> :: <type>``                   |
+----------------+------------------------------------------------------------+
| A type is a type level value which can be specified as a type               |
| identifier or a value composed using type functions.                        |
+-----------------------------------------------------------------------------+

+--------------------+--------------------------------------------------------+
| Identifier         | ::                                                     |
|                    |                                                        |
|                    |   v :: Int                                             |
|                    |   v = 10                                               |
+--------------------+--------------------------------------------------------+
| Expression         | ::                                                     |
|                    |                                                        |
|                    |   v = 10 :: Int                                        |
+--------------------+--------------------------------------------------------+
| Typed Holes (GHC 7.8.1)                                                     |
+-----------------------------------------------------------------------------+
| Use ``_`` wildcard in place of a value to indicate a type hole. GHC         |
| will report the inferred type of the value to be used in place of the hole. |
+--------------------+--------------------------------------------------------+
| Typed hole         | ::                                                     |
|                    |                                                        |
|                    |  v :: Int                                              |
|                    |  v = _ + 10                                            |
+--------------------+--------------------------------------------------------+

Type Operators
~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| ``->`` is a left associative type operator. It takes a functions            |
| `argument type` and `return type` as operands and generates a function type.|
| It is used to generate type signatures of functions from the argument types |
| and the return type of the function.                                        |
+-----------------------------------------------------------------------------+
| A function taking an `Int` argument `x` and returning an `Int`              |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  inc :: (->) Int Int    -- function form                                    |
|  inc :: Int -> Int      -- operator form                                    |
|  inc x = x + 1                                                              |
+-----------------------------------------------------------------------------+
| A function taking two `Int` arguments `x` and `y` and returning an `Int`    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  add :: (->) Int ((->) Int Int)  -- function form                           |
|  add :: Int -> (Int -> Int)      -- explicit left associative form          |
|  add :: Int -> Int -> Int        -- commonly used form                      |
|  add x y = x + y                                                            |
+-----------------------------------------------------------------------------+

Data Types
----------

Defining New Data Types
~~~~~~~~~~~~~~~~~~~~~~~

::

  data Pair   = Pair Int Int deriving (Show, Eq)       -- Product
  data RPair  = RPair { first :: Int, second :: Int }  -- Record
  data Count  = Red Int | Green Int                    -- Sum
  data List a = Empty | Cons a (List a)                -- Recursive

Constructing Data
~~~~~~~~~~~~~~~~~

+---------------------------------------------------+
| Use the constructor on RHS                        |
+---------------------------------------------------+
| ::                                                |
|                                                   |
|   let pair  = Pair 10 20                          |
|   let pair  = RPair 10 20                         |
|   let pair  = RPair {first=10, second=20}         |
|   let count = Red 5                               |
|   let list  = Cons 10 (Cons 20 Empty) :: List Int |
+---------------------------------------------------+

Deconstructing Data by Pattern Match
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A pattern match uses data constructor functions as patterns on LHS to       |
| deconstruct the corresponding algebraic data into its components.           |
+-----------------------------------------------------------------------------+
| Patterns matches in `case` and `function definition` are strict.            |
+-----------------------------------------------------------------------------+
| Patterns matches in `let` and `where` are lazy.                             |
+-----------------------------------------------------------------------------+

Decomposing Product Types
^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let pair = Pair 10 20                                                     |
+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case pair of                        |  total (Pair a b) = a + b            |
|    Pair a b -> a + b                 |                                      |
+--------------------------------------+--------------------------------------+
| Let                                  | Where                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  let Pair a b = pair                 |  total = a + b                       |
|  in a + b                            |   where Pair a b = pair              |
+--------------------------------------+--------------------------------------+

Matching Sum Types
^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  let count = Red 5                                                          |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Since sum type has more than one constructor, the pattern match may fail at |
| run time with a non-exhaustive pattern match error if we do not cover all   |
| constructors.                                                               |
+-----------------------------------------------------------------------------+
| Patterns are matched from top to bottom.                                    |
+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case count of                       |  name Red   i = "R " ++ show i       |
|    Red   i -> "R " ++ show i         |  name Green i = "G " ++ show i       |
|    Green i -> "G " ++ show i         |                                      |
+--------------------------------------+--------------------------------------+

+-----------------------------------------------------------------------------+
| `let` and `where` patterns will always be non-exhaustive for sum types as we|
| can match only one constructor. The pattern match will fail at run time if  |
| the data does not match the specified constructor.                          |
+--------------------------------------+--------------------------------------+
| Let                                  | Where                                |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  let Red i = count                   |  reds = "R " ++ show i               |
|  in "R " ++ show i                   |    where Red i = count               |
|                                      |                                      |
|  -- this match will fail             |  -- this match will fail             |
|  let Green i = count                 |  greens = "G " ++ show i             |
|  in "G " ++ show i                   |    where Green = count in "green"    |
+--------------------------------------+--------------------------------------+

More on Pattern Matches
^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Pair = Pair ((Int, Int), (Int, Int))                                  |
|  let  pair = Pair ((1, 2), (3, 4))                                          |
+-------------------------+---------------------------------------------------+
| Pattern in pattern      | ``total (Pair a (i, j))   = i + j``               |
+-------------------------+---------------------------------------------------+
| Wild card (``_``) match | ``total (Pair _ (i, j))   = i + j``               |
+-------------------------+---------------------------------------------------+
| As pattern              | ``total (Pair a b@(i, j)) = (i + j, b)``          |
+-------------------------+---------------------------------------------------+

Basic Algebraic Data Types (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* TODO: provide links to the definitions in base

::

  data []   a = []    | :    a (List a)                -- Recursive

Note that Haskell's built-in list is not really a special syntax it is a user
defined data type, '[]' is the empty list constructor and ':' is the Cons
constructor. Though there is a syntactic sugar to specify lists in a more
convenient way [1, 2] is equivalent to 1 : 2 : [].

+----------+----------------------------------+-------------------------------+
| Type     | Values                           | Description                   |
+==========+==========+==========+============+===============================+
| ()       | ()       |          |            | Void value or empty tuple     |
+----------+----------+----------+------------+-------------------------------+
| (a, b)   | (1, 'a') | (0.3, 1) | (1, 2)     | Tuple of mixed types          |
+----------+----------+----------+------------+-------------------------------+
| [a]      | []       | 1 : []   | 1 : 2 : [] | List of Int                   |
|          |          |          |            | Explicit constructor syntax   |
|          +----------+----------+------------+-------------------------------+
|          | []       | [1]      | [1,2]      | Sugared syntax                |
|          +----------+----------+------------+-------------------------------+
|          | []       | ['a']    | ['a','b']  | List of chars (String)        |
|          +----------+----------+------------+-------------------------------+
|          | ""       | "a"      | "ab"       | String literals               |
+----------+----------+----------+------------+-------------------------------+
| Ordering | LT       | EQ       | GT         |                               |
+----------+----------+----------+------------+-------------------------------+
| Bool     | True     | False    |            |                               |
+----------+----------+----------+------------+-------------------------------+

Definition Equations
--------------------

+-----------------------------------------------------------------------------+
| All identifier names must start with a lower case letter or ``_``.          |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| A non-function definition equation gives a name to a value.                 |
+-----------------------------------------------------------------------------+
| k = 10                                                                      |
+-----------------------------------------------------------------------------+
| v = k * 2^10                                                                |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Function definition in a single equation form.                              |
+-----------------------------------------------------------------------------+
| sumOfSquares x y = x * x + y * y                                            |
+-----------------------------------------------------------------------------+

Top level Definitions
~~~~~~~~~~~~~~~~~~~~~

Definitions which are not inside any other definition are called `top level
definitions`. A top level definiton can be a function or non-function
definition.

Local Definitions
~~~~~~~~~~~~~~~~~

* let, where
* let in a do block
* let indentation
* where in a do block - cannot refer to bindings extracted from a monad

Anonymous Functions
~~~~~~~~~~~~~~~~~~~

* lambda

Indentation - Layout Rule
~~~~~~~~~~~~~~~~~~~~~~~~~

* http://stackoverflow.com/questions/18024924/haskell-why-is-a-multi-line-let-expression-a-syntax-error

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

Expressing Conditions
---------------------

* case is the source of all conditions

+-----------------------------------------------------------------------------+
| Function definition in multiple equation (pattern matching) form. Each      |
| equation defines the function for a subset of its inputs.                   |
+-----------------------------------------------------------------------------+
* pattern matched defs
* matching order top to bottom
* ignore value with _
+-----------------------------------------------------------------------------+

* guarded defs (in conditional section?)

* case statement
* if statement
* guards

  * wherever pattern matches are used? let?
  * function defs
  * case expression
  * list comprehensions

Function Applications
---------------------

+---------------+--------------+
| Definition    | Application  |
+===============+==============+
| f a b c = ... | v = f x y z  |
+---------------+--------------+

Function Application (built-in)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `Space` is highest precedence and left associative function application     |
+-----------------------------------------------------------------------------+
| f x                                                                         |
+---------+-------------------------------------------------------------------+
| f x y   | (f x) y                                                           |
+---------+-------------------------------------------------------------------+
| f x y z | ((f x) y) z                                                       |
+---------+-------------------------------------------------------------------+

Function Application (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| * $ is just opposite of space i.e. lowest precedence and right associative. |
| * Think evaluating everything after a $ before applying it to the function  |
|   before it.                                                                |
+-------------+---------------------------------------------------------------+
| f $ x       | f x                                                           |
+-------------+---------------------------------------------------------------+
| f $ g x     | f (g x)                                                       |
+-------------+---------------------------------------------------------------+
| f $ g $ h x | f (g (h x))                                                   |
+-------------+---------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| & is reverse function application                                           |
+-----------+-----------------------------------------------------------------+
| x & f     | f x                                                             |
+-----------+-----------------------------------------------------------------+
| x & g & f | f (g x)                                                         |
+-----------+-----------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| swap the arguments before applying                                          |
+--------------+--------------------------------------------------------------+
| flip f $ x y | f y x                                                        |
+--------------+--------------------------------------------------------------+

Function Composition (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| * ``.`` is composition, lower precedence than function application and      |
|   higher precedence than ``$``.                                             |
| * Note ``(f . g . h)`` applies ``h`` to the argument and then feeds the     |
|   result to ``g`` which feeds the result to ``f``.                          |
+-------------------+---------------------------------------------------------+
| (f . g) x         | f (g x)                                                 |
+-------------------+---------------------------------------------------------+
| f . g $ x         | (f . g) x                                               |
+-------------------+---------------------------------------------------------+
| (f . g . h) x     | f $ g $ h x                                             |
+-------------------+---------------------------------------------------------+
| f . g x           | f . (g x)                                               |
+-------------------+---------------------------------------------------------+

Do Expression
-------------

TBD

Defining Modules
----------------

TBD - module declaration: module X where ...

Lists
~~~~~

* List comprehensions
* See prelude for list functions

References
----------

* https://www.haskell.org/hoogle/ One stop shop for any help including keywords
* https://wiki.haskell.org/Keywords Description of all keywords
* https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
* https://hackage.haskell.org/package/base
* https://hackage.haskell.org/ All Haskell packages and their documentation

* Its a good idea to get familiar with Prelude and then other modules in the
  base package after you are familiar with the basic syntax.

