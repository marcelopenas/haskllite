# Haskllite

[![Compilation Status](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)

## Run

    runghc main.hs [arguments]

## Diagrama sintático

![Diagrama Sintático](img/diagram.png)

## EBNF

```ebnf
PROGRAM = { STATEMENT };

STATEMENT = ((IDENTIFIER, "=", EXPRESSION) | (PRINT, "(", EXPRESSION, ")") | ε), EOL;

EXPRESSION = TERM, { ( "+" | "-" ) , TERM };

TERM = FACTOR, { ( "*" | "/" ), FACTOR };

FACTOR = ("+" | "-"), FACTOR | "(", EXPRESSION, ")" | NUMBER;

NUMBER = DIGIT, { DIGIT };

DIGIT = 0 | 1 | ... | 9;

IDENTIFIER = LETTER, {LETTER | DIGIT | "_"};

LETTER = a | b | ... | z | A | B | ... | Z;
```

## Tagging

vX.Y.Z - For normal

xX.Y.Z - For extra

git tag `tag`

git push origin `tag`
