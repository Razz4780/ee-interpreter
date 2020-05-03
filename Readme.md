# ee language interpreter
ee is a simple imperative programming language. It is statically typed. It's grammar is described in the Grammar/grammar.cf file.

## Variable types
1. int
2. bool
3. str
4. functions
Types have no default values, as all variables have to be initialized during declaration. All types are immutable.

## Operators
1. Aritmetic: ```+ - * / %```
2. Relational: ```< <= > >= == !=```
3. Logical: ```&& || !```

## Comments
Single-line comments start with ```//```.

## Program structure
A program is a list of statements which are executed in turn.

## Identifiers
Identifiers are names for variables. Identifiers consist of english letters, digits and
underscores. Identifiers do not start with a digit.

## Statements
Variable definition
```let <variable name> = <expression>;```
Empty
```;```
Block
```
{
    <statement 1>
    <statement 2>
    ...
}
```
Assignment
```<variable name> = <expression>;```
Return
```return [<expression>];```
If elif else
```if (<expression>) <block> (elif (<expression>) <block>)* [else <block>]```
While
```while (<expression>) <block>```
Expression
```<expression>;```

## Boolean expressions
Boolean expressions are evaluated lazily.

## Predefined
There are 4 predefined variables:
```
let printInt(val: int) -> void = ...;
let printStr(val: str) -> void = ...;
let readInt() -> int = ...;
let readStr() -> str = ...;
```
These functions interact with standart output and input.

Additionally when entering function body, special variable ```this``` is defined. It stores the current function (for recursive calls).

## Runtime errors
Runtime errors can occur when:
1. Dividing by ```0```
2. Mod by ```0```
3. Failed parsing inside ```readInt``` function

## Features
1. Static types
2. Nested functions
3. Lambdas
Total points maximum: 30