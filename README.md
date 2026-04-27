# μProver

μProver is a toy implementation of System F intended to demonstrate the Curry-Howard isomorphism.

## Syntax

The syntax of μProver is defined below. Identifers are strings of alphanumeric characters and the
underscore, starting with a non-numeric character (e.g., `foo` or `_23`).

```
term ::=
  | identifier
  | term-abstraction
  | term-application
  | type-abstraction
  | type-application
  | ascription
  | assertion

term-abstraction ::=
  | '(' identifier (':' type)? ')' '=>' term

term-application ::=
  | term term

type-abstraction ::=
  | '[' identifier (',' identifier)* ']' '=>' term

type-application ::=
  | term '[' type (',' type)* ']'

ascription ::=
  | term ':' type

assertion ::=
  | '!'

type ::=
  | identifier
  | arrow
  | forall
  | unit
  | '_'

type arrow ::=
  | type -> type
  | '[' identifier (',' identifier)* ']' => type

type unit ::=
  | '(' ')'
```