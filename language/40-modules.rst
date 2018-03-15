.. raw:: html

  <style> .red {color:red} </style>
  <style> .blk {color:black} </style>
  <style> .center { text-align: center;} </style>
  <style> .strike { text-decoration: line-through;} </style>

.. role:: strike
.. role:: center

.. role:: red
.. role:: blk

Basic Syntax
============

.. contents:: Table of Contents
   :depth: 1

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
| Module   | Haskell code is arranged in modules of related functionality.    |
|          | Each module exports symbols (functions, types etc) which can be  |
|          | imported by the user to make use of the functionality provided   |
|          | by the module.                                                   |
+----------+------------------------------------------------------------------+
| packages | A package is a collection of modules. Packages can be installed  |
|          | independently. Some packages (e.g. base) are installed with the  |
|          | compiler.                                                        |
+----------+------------------------------------------------------------------+
| base     | `base` is a package providing basic and essential functionality  |
+----------+------------------------------------------------------------------+
| Prelude  | A module from `base` package providing the bare necessities and  |
|          | imported implicitly.                                             |
+----------+------------------------------------------------------------------+
| Scrutinee| In a `case` construct the expression on which we are pattern     |
|          | matching.                                                        |
+----------+------------------------------------------------------------------+

Comments
--------

::

  -- This is single line comment.
  {-
    This is a multiline
    block comment.
  -}

Files & Modules
---------------

Filenames
~~~~~~~~~

+-----------+------------------+
| Extension | Meaning          |
+-----------+------------------+
| .hs       | Haskell          |
+-----------+------------------+
| .lhs      | Literate Haskell |
+-----------+------------------+

Using Modules: Importing Names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An import directive brings `names` from other modules into the scope of the
importing module.

Names may be qualified or unqualified. A qualified name has a module name
prefixed to it e.g. ``Data.List.take`` means the ``take`` name defined in
``Data.List`` module..

Multiple import statements can be used to import different sets of names from
the same module in different ways.

+---------------------------------------------------------------------------------------+
| Assuming module ``X.Y`` defines names ``a``, ``b`` and ``c``.                         |
+---------------------------------+--------------------------------+--------------------+
| import directive                | Description                    | What names come    |
|                                 |                                | into scope         |
+=================================+================================+====================+
| import X.Y                      | imports everything             | a, b, c,           |
|                                 |                                | X.Y.a, X.Y.b, X.Y.c|
+---------------------------------+--------------------------------+--------------------+
| import X.Y ()                   | Imports only orphan instances  |                    |
+---------------------------------+--------------------------------+--------------------+
| import X.Y (a, b)               | import only ``a`` and ``b``    | a, b,              |
|                                 |                                | X.Y.a, X.Y.b       |
+---------------------------------+--------------------------------+--------------------+
| import X.Y hiding (c)           | import everything except ``c`` | a, b,              |
|                                 |                                | X.Y.a, X.Y.b       |
+---------------------------------+--------------------------------+--------------------+
| import qualified X.Y (a, b)     | ``a`` and ``b`` only qualified | X.Y.a, X.Y.b       |
+---------------------------------+--------------------------------+--------------------+
| import X.Y as Z                 | same as ``import X.Y`` except  | ``a``, ``b``,      |
|                                 | that it is renamed to Z        | ``c``, ``Z.a``,    |
|                                 |                                | ``Z.b``, ``Z.c``   |
+---------------------------------+--------------------------------+--------------------+
| import qualified X.Y as Z       | same as                        | ``Z.a``, ``Z.b``,  |
|                                 | ``import qualified X.Y``       | ``Z.c``            |
|                                 | except that it is renamed to Z | ``Z.c``            |
+---------------------------------+--------------------------------+--------------------+
| All instances of a type are automatically imported along with the type                |
| or the typeclass. Since orphan instances have no type or typeclass defined in the     |
| same module, they are always imported unconditionally.                                |
+---------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Importing types                                                             |
+=================================+===========================================+
| import X.Y (A)                  | import type A                             |
+---------------------------------+-------------------------------------------+
| import X.Y (A(C1,C2))           | import A and its data                     |
|                                 | constructors C1 and C2 as well            |
+---------------------------------+-------------------------------------------+
| import X.Y (A(a1, a2))          | import A and its memmber                  |
|                                 | functions (when A is a                    |
|                                 | typeclass) or selector                    |
|                                 | functions (when A is a record)            |
|                                 | a1 and a2                                 |
+---------------------------------+-------------------------------------------+
| import X.Y (A(..))              | import type A and all its                 |
|                                 | data constructors,                        |
|                                 | selector or member functions              |
+---------------------------------+-------------------------------------------+
| Note the above import won't bring in the data constructor if A is defined as|
| a pattern and not a data constructor.                                       |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| importing type level or data level operators                                |
+-----------------------------------------------------------------------------+
| operators do not have a upper or lower case based distinction, so the same  |
| named operator could be a type operator as well as a function operator.     |
+=================================+===========================================+
| import ((:=))                   | import operator `:=`, if a                |
|                                 | type as well as a function                |
|                                 | with this name exist, then only           |
|                                 | the function will be imported             |
+---------------------------------+-------------------------------------------+
| When the operator is a type, then the above import won't import a           |
| constructor or pattern of the same name if it exists.                       |
+---------------------------------+-------------------------------------------+
| import (type (:=))              | import type operator `:=`                 |
|                                 | requires -XExplicitNamespaces             |
+---------------------------------+-------------------------------------------+
| import (pattern (:=))           | import pattern and/or                     |
|                                 | constructor                               |
|                                 | `:=` (-XPatternSynonyms)                  |
+---------------------------------+-------------------------------------------+
| Allows import of data constructor without its parent type constructor       |
+-----------------------------------------------------------------------------+


+-----------------------------------------------------------------------------+
| Others (importing from named packages, importing safe)                      |
+=================================+===========================================+
| import "base" Data.List         | from package "base"                       |
|                                 | -XPackageImports                          |
+---------------------------------+-------------------------------------------+
| import safe  Data.List          | Safe Haskell (-XSafe, -XUnsafe            |
|                                 | -XTrustworthy). Since 7.2                 |
+---------------------------------+-------------------------------------------+

* By default, Prelude is implicitly imported. However, if you add an
  explicit import declaration for Prelude, implicit import gets turned off::

    import Prelude hiding (zip)
    import qualified Prelude as P

* Orphan instances are those which are not defined in the same file as the
  typeclass or the type.

Defining Modules: Exporting Names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-----------------------------------------------------------------------------+
| Assuming module ``X.Y`` defines names ``a``, ``b`` and ``c``.               |
+-----------------------------+-----------------------------------------------+
| module X.Y where ...        | Exports all names i.e. ``a``, ``b``, ``c``    |
+-----------------------------+-----------------------------------------------+
| module X.Y () where ...     | Only orphan instances (if any) are exported   |
+-----------------------------+-----------------------------------------------+
| module X.Y (a, b) where ... | Exports names ``a`` and ``b``                 |
+-----------------------------+-----------------------------------------------+

Instances are always exported along with the type or the typeclass. Since
orphan instances have no type or typeclass associated with them they are always
automatically exported.

+-----------------------------------------------------------------------------+
| Exporting types                                                             |
+============================+================================================+
| module X.Y (A) where ...   | Export name ``A`` along with any instances     |
+----------------------------+------------------------------------------------+
| module X.Y (A) where ...   | Export name ``A`` along with any instances     |
+----------------------------+------------------------------------------------+
| module X.Y (A(C1,C2))      | Export A and its data                          |
|                            | constructors C1 and C2 as well                 |
+----------------------------+------------------------------------------------+
| module X.Y (A(a1, a2))     | Export A and its memmber                       |
|                            | functions (when A is a                         |
|                            | typeclass) or selector                         |
|                            | functions (when A is a record)                 |
|                            | a1 and a2                                      |
+----------------------------+------------------------------------------------+
| module X.Y (A(..))         | Export type A and all its                      |
|                            | data constructors,                             |
|                            | selector or member functions                   |
+----------------------------+------------------------------------------------+

+-----------------------------------------------------------------------------+
| Exporting type level or data level operators                                |
+-----------------------------------------------------------------------------+
| Operators do not have an upper or lower case based distinction, so the same |
| named operator could be a type operator as well as a function operator.     |
+=================================+===========================================+
| module ((:=))                   | export operator `:=`, if a                |
|                                 | type as well as a function                |
|                                 | with this name exist, then only           |
|                                 | the function will be exported             |
+---------------------------------+-------------------------------------------+
| When the operator is a type, then the above export won't export a           |
| data constructor or pattern of the same name if it exists.                  |
+---------------------------------+-------------------------------------------+
| module (type (:=))              | export type operator `:=`                 |
|                                 | requires -XExplicitNamespaces             |
+---------------------------------+-------------------------------------------+
| module (pattern (:=))           | export pattern and/or                     |
|                                 | data constructor                          |
|                                 | `:=` (-XPatternSynonyms)                  |
+---------------------------------+-------------------------------------------+
| Allows export of data constructor without its parent type constructor       |
+-----------------------------------------------------------------------------+

+-----------------------------------------------------------------------------+
| Re-exporting imported names.                                                |
+-----------------------------------------------------------------------------+
| The form ``module M`` in export list names the set of all names that are in |
| scope with both an unqualified name ``a`` as well as a qualified name       |
| ``M.a``.                                                                    |
+----------------------------+------------------------------------------------+
| ::                         |                                                |
|                            |                                                |
|  module X.Y (module X.Y    | Export all names from module X.Y itself and    |
|             , module Z)    | all names from module Z too.                   |
|  where ...                 |                                                |
|  import Z                  |                                                |
+----------------------------+------------------------------------------------+
| ::                         |                                                |
|                            |                                                |
|  module X.Y (module R)     | Export all names from module ``Z``             |
|  where ...                 |                                                |
|  import Z as R             |                                                |
+----------------------------+------------------------------------------------+
| ::                         |                                                |
|                            |                                                |
|  module X.Y (module R)     | Nothing will be exported because no            |
|  where ...                 | unqualified names from R are in scope.         |
|  import qualified Z as R   |                                                |
+----------------------------+------------------------------------------------+
| ::                         |                                                |
|                            |                                                |
|  module X.Y (R.a)          | Name ``a`` from module ``R`` will be exported  |
|  where ...                 |                                                |
|  import qualified Z as R   |                                                |
+----------------------------+------------------------------------------------+

+-----------------------------------------------------------------------------+
| The unqualified names of the entities exported by a module must all be      |
| distinct (within their respective namespace).                               |
+---------------------------------+-------------------------------------------+
| ::                              |                                           |
|                                 |                                           |
|  module A (C.f, module B) where | Invalid: two exported names C.f and B.f   |
|  import B(f)                    | have same unqualified names.              |
|  import qualified C(f)          |                                           |
+---------------------------------+-------------------------------------------+

The ``Main`` Module
~~~~~~~~~~~~~~~~~~~

* ``Main`` is special module name. When compiled, the ``Main`` module is linked
  to create an executable.
* A file without a module declaration is automatically considered to contain a
  ``Main`` module.

Namespaces
~~~~~~~~~~

All keywords, functions, variables start with lowercase letters

+-----------------------------------------------------------------------------+
| The following identifiers start with `uppercase` letters                    |
+--------------------+-------------------+------------------------------------+
| Module identifiers | Types             | Data constructors                  |
+--------------------+-------------------+------------------------------------+
| These three namespaces can use the same identifier names without conflict.  |
+-----------------------------------------------------------------------------+
| ::                                                                          |
|                                                                             |
|  -- 'Play' refers to three distinct objects in three distinct namespace     |
|  module Play where       -- module name                                     |
|  data Play =             -- type                                            |
|       Play Int           -- data constructor                                |
|                                                                             |
|  class Clay where ...    -- type (typeclass)                                |
+-----------------------------------------------------------------------------+

Term Level Namespace
^^^^^^^^^^^^^^^^^^^^

Definitions defining identifiers without any preceding keywords.

Type Level Namespace
^^^^^^^^^^^^^^^^^^^^

Definitions with type, newtype or data keywords. In a data or newtype
definition, constructors are term level whereas their arguments are type level.
Constructors are somewhat special as they are a bridge or binding between type
and term level.

* Everything in a type synonym is in type level namespace.
* In a type signature everything on the right side of a `::` is in type level
  namespace.
* Everything in a class declaration is type level except the default
  definitions of the class functions.

Kind level Namespace
^^^^^^^^^^^^^^^^^^^^

Whenever we use a `::` on a type then everything on the right side of
`::` is in kind level namespace.

Wherever a type can be used a kind can also be used. To remove the ambiguity in
such cases we start the kind level identifiers with a `'`. In a type context if
an identifier does not start with `'` or is not on the right side of a `::`
then it is considered a type otherwise it is considered a kind.

Do not confuse the kind level `'` or `''` used in template haskell context.

Pragmas
~~~~~~~

Language pragmas must be declared on top before module declaration.

Notational Conventions
----------------------

+------------------------+--------------+
| concrete values        | x, y, z      |
+------------------------+--------------+
| plural concrete values | xs, ys, zs   |
+------------------------+--------------+
| functions              | f, g, h      |
+------------------------+--------------+
| types                  | a, b, c      |
+------------------------+--------------+

References
----------

* https://www.haskell.org/hoogle/ One stop shop for any help including keywords
* https://wiki.haskell.org/Keywords Description of all keywords
* https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
* https://hackage.haskell.org/package/base
* https://hackage.haskell.org/ All Haskell packages and their documentation
* https://www.haskell.org/onlinereport/haskell2010/ The Haskell Specification
* http://blog.ploeh.dk/2015/08/17/when-x-y-and-z-are-great-variable-names/
* http://degoes.net/articles/insufficiently-polymorphic Descriptive Variable Names: A Code Smell
* https://wiki.haskell.org/Import

* Its a good idea to get familiar with Prelude and then other modules in the
  base package after you are familiar with the basic syntax.

