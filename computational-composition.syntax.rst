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
