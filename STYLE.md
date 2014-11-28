## Coding Standards

### Imports

  - Place imports of external modules above imports of modules from this project
  - Always use explicit import lists rather than importing a whole module
  - Qualified import of a whole module is ok (this doesn't pollute the current module namespace)
  - When writing import lists, the order of items in the list should be:
    1. Type classes
    2. Datatypes and constructors
    3. Ordinary functions and variables
  - When writing a qualified import of a module, if also explicitly importing some things from the module with an import list, place the qualified import directly above the import list.
  - If there is a choice between importing the Prelude version of something, and the "real" version in a module, then the non-Prelude version always wins. For example, import Data.List.map rather than Prelude.map
  - When importing functions, import operators before named functions
  - If an import list contains only one kind of thing (e.g. only type classes, or only functions) it is OK to put it on one line
  - In general import lists should contain *exactly* what is used in the module and no more
    - As an exception, always import type classes and data types fully, because it is confusing when only some pieces of one of those are visible

