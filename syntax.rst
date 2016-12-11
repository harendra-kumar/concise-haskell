Syntax in a Nutshell
====================

+-----------------------------------------------------------------------------+
| This is arranged in a strict dependency based sequence as much as possible, |
| later sections building on previous ones to ensure a smooth sequential      |
| reading flow.                                                               |
+-----------------------------------------------------------------------------+
| Roadmap (TBD)                                                               |
+-----------------------------------------------------------------------------+

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
| Scope    | Scope of visibility of variable bindings.                        |
+----------+------------------------------------------------------------------+

Composing Expressions
---------------------

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
+-----------+-----------+-----------------------------------------------------+
| print 'a' | even 10   | subtract 0.5 10.5                                   |
+-----------+-----------+-----------------------------------------------------+
| These functions are called `prefix functions` since the function name comes |
| before its arguments.                                                       |
+-----------------------------------------------------------------------------+
| We can also use operators in an expression. A `binary operator` is an       |
| `infix function` as the function name is placed in the middle of its two    |
| arguments.                                                                  |
+---------+---------+---------------------------------------------------------+
| 5 + 4   | 5 - 4   | 2^3                                                     |
+---------+---------+---------------------------------------------------------+
| It becomes obvious that they are really functions if we use them in prefix  |
| notation:                                                                   |
+---------+---------+---------------------------------------------------------+
| (+) 5 4 | (-) 5 4 | (^) 2 3                                                 |
+---------+---------+---------------------------------------------------------+
| A Haskell expression is composed using functions, operators and values.     |
| Arguments of functions could be expressions themselves. Argument            |
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

Arithmetic Operations (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Defined in base
* TODO: point to prelude itself
* TODO: show the result of the expression

+-----------+----------------+------------------------------------------------+
| Operation | Example        | Description                                    |
+===========+================+================================================+
| \+        | 3 + 2          | Addition                                       |
+-----------+----------------+------------------------------------------------+
| \-        | 3 - 2          | Subtraction                                    |
+-----------+----------------+------------------------------------------------+
| \*        | 3 * 2          | Multiplication                                 |
+-----------+----------------+------------------------------------------------+
| /         | 3 / 2          | Fractional division                            |
+-----------+----------------+------------------------------------------------+
| ^         | 3 ^ 2          | Positive integral power                        |
+-----------+----------------+------------------------------------------------+
| ^^        | 3 ^^ 2         | Integral power                                 |
+-----------+----------------+------------------------------------------------+
| \**       | 3 \** 2.2      | Fractional power                               |
+-----------+----------------+------------------------------------------------+
| div       | 3 \`div\` (-2) | Integral division truncated towards negative   |
|           |                | infinity                                       |
+-----------+----------------+------------------------------------------------+
| mod       | 3 \`mod\` (-2) | modulus of `div`                               |
+-----------+----------------+------------------------------------------------+
| quot      | 3 \`div\` (-2) | Integral division quotient truncated towards   |
|           |                | zero                                           |
+-----------+----------------+------------------------------------------------+
| rem       | 3 \`div\` (-2) | remainder of `quot`                            |
+-----------+----------------+------------------------------------------------+

Function Application (built-in)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `whitespace` or `juxtaposition` is highest precedence and left associative  |
| function application                                                        |
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
| Think evaluating everything after a `$` before applying it to the function  |
| preceding it.                                                               |
+-------------+---------------------------------------------------------------+
| f $ x       | f x                                                           |
+-------------+---------------------------------------------------------------+
| f $ g x     | f (g x)                                                       |
+-------------+---------------------------------------------------------------+
| f $ g $ h x | f (g (h x))                                                   |
+-------------+---------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| ``&`` is reverse function application i.e. argument is written before the   |
| function.                                                                   |
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

Operators as Functions and Vice Versa (built-in)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------+---------------+
| Prefix      | Infix         |
+=============+===============+
| ``div 4 3`` | ``4 `div` 3`` |
+-------------+---------------+
| ``(+) 4 3`` | ``4 + 3``     |
+-------------+---------------+

+---------------------------------------------+
| Operator Sections                           |
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

Defining Equations
------------------

+-----------------------------------------------------------------------------+
| A definition equation gives a name to an expression:                        |
+-----------+-----------------------------------------------------------------+
| In a file | ``<identifier> = <expression>``                                 |
+-----------+-----------------------------------------------------------------+
| In GHCi   | ``let <identifier> = <expression>``                             |
+-----------+-----------------------------------------------------------------+
| All identifier names must start with a lower case letter or ``_``.          |
+-----------------------------------------------------------------------------+

Definitions allow you to:

* break bigger expressions into smaller ones
* define reusable expressions

Top level Definitions
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Definitions which are not nested inside any other definition are called     |
| `top level definitions`.                                                    |
+-----------------------------------------------------------------------------+
| Top level definitions have a global scope which means the identifiers bound |
| by these equations are visible to all other equations and their nested      |
| local scopes in the file.                                                   |
+-----------------------------------------------------------------------------+
| ``k = 10``                                                                  |
+-----------------------------------------------------------------------------+
| ``v = k * 2^10``                                                            |
+-----------------------------------------------------------------------------+

Nested Local Definitions
~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A `let` or `where` clause defines a local scope. Variables introduced in a  |
| local scope are not visible in parent or sibling equation scopes.           |
+-----------------------------------------------------------------------------+
| Bindings in a local scope will shadow bindings of the same name from parent |
| scopes.                                                                     |
+-----------------------------------------------------------------------------+
| Multiple equations can be defined in a single `let` or `where` clause just  |
| like at the top level.                                                      |
+-----------------------------------------------------------------------------+

Expression Local (let)
^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| A `let` clause is an expression with one or more local definitions.         |
+-----------------------------------------------------------------------------+
| Since `let` is an expression it can be used wherever an expression can be   |
| used.                                                                       |
+-----------------------------------------------------------------------------+
| Bindings introduced by let are visible only in the let expression.          |
+-----------------------------------------------------------------------------+
| ``10 + let x = 5 in x * x + 2^3``                                           |
+-----------------------------------------------------------------------------+
| ``let x = 1 in let y = 2 in let z = 3 in x + y + z``                        |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let x = 1                                                                 |
|       y = 2                                                                 |
|       z = 3                                                                 |
|   in x + y + z                                                              |
+-----------------------------------------------------------------------------+

Equation Local (where)
^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| A `where` clause defines one or more equations in a local scope             |
| of another equation.                                                        |
+-----------------------------------------------------------------------------+
| A `where` clause is not an expression in itself therefore unlike `let` it   |
| cannot be embedded arbitrarily inside an expression. It can only be used    |
| after as part of an equation definition.                                    |
+-----------------------------------------------------------------------------+
| Bindings introduced by where are visible only in the local scope of the     |
| equation it is defined in.                                                  |
+-------------------------+---------------------------------------------------+
| ::                      | ::                                                |
|                         |                                                   |
|  n = x + y + z          |  n = x                                            |
|    where x = 1          |     where x = y + 1                               |
|          y = 2          |              where y = z + 2                      |
|          z = 3          |                       where z = 3                 |
+-------------------------+---------------------------------------------------+

Equation Indentation Rule
~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| When you are writing multiline equations or multiple equations in GHCi or in|
| a file you need to ensure that they are `properly indented`.                |
+-----------------------------------------------------------------------------+
| All equations at a given scope (`top level`, `let` or `where`) must start   |
| in the same column.                                                         |
| A multiline equation can continue in an arbitrary column as long as it is   |
| indented at least one column beyond the start column of the equation.       |
+-----------------------------------------------------------------------------+
| A `do` expression block has a few more rules described later.               |
+-----------------------------------------------------------------------------+

Defining Functions
------------------

+--------------+---------------+
| Application  | Definition    |
+==============+===============+
| v = f x y z  | f a b c = ... |
+--------------+---------------+

+-----------------------------------------------------------------------------+
| Function definition equations                                               |
+-----------------------------------------------------------------------------+
| ``square n = n * n``                                                        |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = square x + square y``                                  |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = let square n = n * n in (square x + square y)``        |
+-----------------------------------------------------------------------------+
| ``sumOfSquares x y = (square x + square y) where square n = n * n``         |
+-----------------------------------------------------------------------------+

Anonymous Functions
~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A lambda is an expression denoting a function. It allows you to define a    |
| function in-place inside an expression.                                     |
+-----------------------------------------------------------------------------+
| ``\a b c -> ...``                                                           |
+-----------------------------------------------------------------------------+
| ``let sumOfSquares f x y = f x + f y in sumOfSquares (\n -> n * n) 3 4``    |
+-----------------------------------------------------------------------------+

Ad-hoc Functions
----------------

Previously we defined new functions which passed their inputs to a composition
of existing functions without looking at it.  We will now define what we call
`ad-hoc functions` which examine their inputs and can define a custom mapping
from inputs to outputs.  Ad-hoc functions are implemented by using case
analysis on its inputs (algebraic data types) and mapping selected values to
desired output values.

+--------------------------+---------------------+----------------------------+
| Data Level               | Bridge              | Type Level                 |
+==========================+=====================+============================+
| Data construction        |                     |                            |
+--------------------------+                     |                            |
| Case analysis            | Data declaration    |                            |
| (Ad-hoc Function)        |                     | Algebraic Data Types       |
+--------------------------+---------------------+----------------------------+

Data Declaration
~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| User defined algebraic data type.                                           |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   data Pair   = Pair Int Int deriving (Show, Eq)       -- Product           |
|   data Count  = Red Int | Green Int                    -- Sum               |
|   data List a = Empty | Cons a (List a)                -- Recursive         |
+-----------------------------------------------------------------------------+

Data Construction
~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Use a data constructor function to create a data reference                  |
+-----------------------------------------------------------------------------+
| x = C a b c ...                                                             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|   let pair  = Pair 10 20                                                    |
|   let count = Red 5                                                         |
|   let list  = Cons 10 (Cons 20 Empty) :: List Int                           |
+-----------------------------------------------------------------------------+

Case Analysis (Ad-hoc Functions)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Case analysis is the opposite of algebraic data type construction. It allows us
to navigate through all the choices (values) represented by the data type. For
each choice or subset of choices we can apply a different transform so as to
map the (input) data type to another (output) data type thus implementing the
mapping represented by the function.

A `case` expression is the only way (except syntactic sugars) to perform a case
analysis on an algebraic data type to implement ad-hoc functions.

Case Expression
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| A `case` expression maps an `<input expr>` to an output expression which    |
| is a function of the input.                                                 |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  case <input expr> of                                                       |
|    C1 a b c ... -> <output expr1>                                           |
|    C2 a b c ... -> <output expr2>                                           |
|    x            -> <output expr3>                                           |
|    ...                                                                      |
+-----------------------------------------------------------------------------+
| The `<input expr>` is called the `scrutinee`.                               |
+-----------------------------------------------------------------------------+
| C1, C2 etc. are the constructors of the type of `<input expr>`. This is the |
| selection of the choice represented by a sum type.                          |
+-----------------------------------------------------------------------------+
| ``a`` ``b`` ``c`` are variables representing the components of the product  |
| type (if any) represented by the chosen constructor.                        |
+-----------------------------------------------------------------------------+
| Selecting a constructor (i.e. the sum type) and selecting the               |
| individual components of the product in the selected constructor (if any)   |
| is called a `pattern match`.                                                |
+-----------------------------------------------------------------------------+
| Patterns can be nested i.e. ``a`` ``b`` ``c`` themselves can be specified   |
| as pattern matches deconstructing them further.                             |
+-----------------------------------------------------------------------------+
| Case alternatives are matched from top to bottom.                           |
+-----------------------------------------------------------------------------+
| If the pattern being tried is a variable (e.g. ``x``) or ``_`` the match    |
| will always succeed (irrefutable). In case of ``_`` the input is discarded  |
| while in case of a variable the input is bound to the variable.             |
+-----------------------------------------------------------------------------+
| The output expression can be another case expression to further deconstruct |
| the retrieved components or an expression to just transform the resulting   |
| value.                                                                      |
+-----------------------------------------------------------------------------+
| All the output expressions must be of the same type i.e. the result type of |
| the case expression.                                                        |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Some important facts about `case` and `pattern match`                       |
+=============================================================================+
| Case is the fundamental way to pattern match in Haskell. All other forms of |
| pattern matches are just syntactic sugar on top of case.                    |
+-----------------------------------------------------------------------------+
| The `scrutinee` of case is strictly evaluated to WHNF to enable the pattern |
| match. This is the only source of all forms of strict evaluation in Haskell.|
+-----------------------------------------------------------------------------+
| `case` is also the fundamental tool to express branching in Haskell.        |
| Branches are needed in general to map inputs to outputs which is done via a |
| case expression in Haskell. There is no other way to express branching.  All|
| other forms of branching are just syntactic sugar on top of case.           |
+-----------------------------------------------------------------------------+

Multi Equation Function Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An ad-hoc function can be defined more naturally as multiple equations each
equation defining the function for a certain input pattern by using a pattern
match on its arguments.  This is just a syntactic sugar on a `case` pattern
match.

+--------------------------------------+--------------------------------------+
| Function                             | Case                                 |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  name Red   i = "R " ++ show i       |  name c = case c of                  |
|  name Green i = "G " ++ show i       |    Red   i -> "R " ++ show i         |
|                                      |    Green i -> "G " ++ show i         |
+--------------------------------------+--------------------------------------+
| All equations of a function must remain together.                           |
+-----------------------------------------------------------------------------+
| As in case alternatives the equations are matched from top to bottom.       |
+-----------------------------------------------------------------------------+
| A multi equation function can also be defined in `let` and `where` clauses. |
+-----------------------------------------------------------------------------+

Pattern Matches
~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| In addition to `case` expression and `function definition` pattern matches  |
| can also be performed in `let` and `where` clauses.                         |
+-----------------------------------------------------------------------------+
| Pattern matches in `case` and `function definition` are strict.             |
+-----------------------------------------------------------------------------+
| Pattern matches in `let` and `where` are lazy or irrefutable. TBD define    |
| irrefutable.                                                                |
+-----------------------------------------------------------------------------+
| Pattern matching rules as specified for `case` apply to other forms as      |
| well.                                                                       |
+-----------------------------------------------------------------------------+

Deconstructing a Product
^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| A product is deconstructed by specifying a variable for each component of   |
| the product.                                                                |
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

Selecting Alternatives of a Sum
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  let count = Red 5                                                          |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Pattern match on a multi-constructor (sum) type may fail at                 |
| run time with a `non-exhaustive pattern match` error if we do not cover all |
| constructors.                                                               |
+-----------------------------------------------------------------------------+
| Patterns are matched from top to bottom in sequence.                        |
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
| Pattern matches in `let` and `where` are lazy or irrefutable. We can match  |
| any or all constructors but it may fail when we use the value belonging to  |
| a non-matching constructor.                                                 |
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
|  in "G " ++ show i                   |    where Green i = count             |
+--------------------------------------+--------------------------------------+

More on Pattern Matches
^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  data Pair = Pair (Int, Int) (Int, Int)                                     |
|  let  pair = Pair (1, 2) (3, 4)                                             |
+-------------------------+---------------------------------------------------+
| Nested pattern          | ``total (Pair a (i, j))   = i + j``               |
+-------------------------+---------------------------------------------------+
| Wild card (``_``) match | ``total (Pair _ (i, j))   = i + j``               |
+-------------------------+---------------------------------------------------+
| `As pattern`            | ``total (Pair a b@(i, j)) = (i + j, b)``          |
| (``b`` as ``(i, j)``)   |                                                   |
+-------------------------+---------------------------------------------------+
| `b` will be bound to the original argument passed and `i` and `j` will be   |
| bound to the deconstructed components of `b`. Pattern match of `b` is       |
| irrefutable since `b` matches the incoming argument as it is.               |
+-----------------------------------------------------------------------------+

Irrefutable Pattern Matches
^^^^^^^^^^^^^^^^^^^^^^^^^^^

+-----------------------------------------------------------------------------+
| Irrefutable means the pattern match is committed. In a multiple alternative |
| case it implies that the alternative is chosen and no more alternatives     |
| will be tried.                                                              |
+-----------------------------------------------------------------------------+

+-------------------------------------+---------------------------------------+
| Irrefutables that cannot fail       | Irrefutables that can fail            |
+=====================================+=======================================+
| Wildcards (``_`` or a variable)     | As patterns                           |
+-------------------------------------+---------------------------------------+
|                                     | Patterns in `let` and `where`         |
+-------------------------------------+---------------------------------------+
|                                     | Patterns marked lazy using ``~``      |
+-------------------------------------+---------------------------------------+
| Note pattern match on a single constructor data type can never fail.        |
+-----------------------------------------------------------------------------+

Basic Algebraic Data Types (Prelude)
------------------------------------

* TODO: provide links to the definitions in base
* Provide the definitions as well

+----------+----------------------------------+-------------------------------+
| Type     | Values                           | Description                   |
+==========+==========+==========+============+===============================+
| Bool     | True     | False    |            |                               |
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
| ()       | ()       |          |            | Void value or empty tuple     |
+----------+----------+----------+------------+-------------------------------+
| (a, b)   | (1, 'a') | (0.3, 1) | (1, 2)     | Two Tuple                     |
+----------+----------+----------+------------+-------------------------------+

Boolean Conditions
------------------

Comparisons resulting in Booleans (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Operations on Booleans (Prelude)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Branching on Booleans
~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| `if` statement is just a syntactic sugar on top of a `case` scrutiny on     |
| `Bool`                                                                      |
+------------------------------------+----------------------------------------+
| ::                                 | ::                                     |
|                                    |                                        |
|  case pred of                      |  if pred                               |
|    True ->  expr1                  |  then expr1                            |
|    False -> expr2                  |  else expr2                            |
+------------------------------------+----------------------------------------+

+-----------------------------------------------------------------------------+
| Guards                                                                      |
+-----------------------------------------------------------------------------+
| Pattern matches select a branch solely based on the constructor             |
| pattern. However, they can always be refined by adding boolean `guards`.    |
| Guards can use deconstructed variables in predicates.                       |
+--------------------------------------+--------------------------------------+
| Case                                 | Function                             |
+--------------------------------------+--------------------------------------+
| ::                                   | ::                                   |
|                                      |                                      |
|  case count of                       |  name Red   i | i < 5 = "R few"      |
|    Red   i | i < 5                   |  name Red   _         = "R many"     |
|            -> "R few"                |  name Green i = "G " ++ show i       |
|    Red _   -> "R many"               |                                      |
|    Green i -> "G " ++ show i         |                                      |
+--------------------------------------+--------------------------------------+

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

Defining Modules
----------------

TBD - module declaration: module X where ...

Lists
~~~~~

::

  data []   a = []    | :    a (List a)                -- Recursive

Note that Haskell's built-in list is not really a special syntax it is a user
defined data type, '[]' is the empty list constructor and ':' is the Cons
constructor. Though there is a syntactic sugar to specify lists in a more
convenient way [1, 2] is equivalent to 1 : 2 : [].

* List comprehensions
* See prelude for list functions

Do Expression
-------------

* TBD
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
| Associative       | ``infix <precedence> <op>``                             |
+-------------------+---------------------------------------------------------+
| Left associative  | ``infixl <precedence> <op>``                            |
+-------------------+---------------------------------------------------------+
| Right associative | ``infixr <precedence> <op>``                            |
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
| $ is just opposite of normal function application (juxtaposition or whitespace) i.e. lowest precedence and right associative.                 |
+-----------------------------------------------------------------------------------------------------------------------------------------------+
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
| ``->`` is a right associative type operator which is used to generate type  |
| signatures of functions. It takes a function's `argument type` and          |
| `return type` as operands and generates a function type.                    |
+-----------------------------------------------------------------------------+
| A function taking an `Int` argument `x` and returning an `Int`:             |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  inc :: (->) Int Int    -- function form                                    |
|  inc :: Int -> Int      -- operator form                                    |
|  inc x = x + 1                                                              |
+-----------------------------------------------------------------------------+
| A multi argument function is really a single argument function returning    |
| another function which consumes the rest of the arguments.                  |
| A function taking two `Int` arguments `x` and `y` and returning an `Int`:   |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  add :: (->) Int ((->) Int Int)  -- function form                           |
|  add :: Int -> (Int -> Int)      -- explicit right associative form         |
|  add :: Int -> Int -> Int        -- commonly used infix form                |
|  add x y = x + y                                                            |
+-----------------------------------------------------------------------------+

Namespaces
----------

+-----------------------------------------------------------------------------+
| Identifiers must start with an `uppercase` letter                           |
+--------------------+-------------------+------------------------------------+
| Module identifiers | Type constructors | Data constructors                  |
+--------------------+-------------------+------------------------------------+
| These three namespaces can use the same identifier names without conflict.  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- 'Play' refers to three distinct objects in three distinct namespace     |
|  module Play where         -- module name                                   |
|  data Play a =             -- type constructor                              |
|       Play a               -- data constructor                              |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Identifiers must start with a `lowercase` letter                            |
+------------------------------------+----------------------------------------+
| type variables                     | term variables                         |
+------------------------------------+----------------------------------------+
| These two namespaces can use the same identifier names without conflict.    |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- identifier 'play' refers to three distinct objects                      |
|  play ::               -- function name                                     |
|       play -> play     -- type variable                                     |
|  play play = play      -- function name (global scoped)                     |
|                        -- parameter name (local scoped)                     |
+-----------------------------------------------------------------------------+

References
----------

* https://www.haskell.org/hoogle/ One stop shop for any help including keywords
* https://wiki.haskell.org/Keywords Description of all keywords
* https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
* https://hackage.haskell.org/package/base
* https://hackage.haskell.org/ All Haskell packages and their documentation

* Its a good idea to get familiar with Prelude and then other modules in the
  base package after you are familiar with the basic syntax.

