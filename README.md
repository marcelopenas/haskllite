# Haskllite

[![Compilation Status](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)

## Run

    runghc main.hs [arguments]

## Diagrama sintático

![Diagrama Sintático](diagram.png)

## EBNF

```ebnf
expr = term, { ( '+' | '-' | '^' ) , term };

term = { exponent, ( '*' | '/' ) }, exponent;

exponent = {factor, ('**')}, factor;

factor = integer | '(', expr, ')';

integer = [ '-' ], digit, { digit };

digit = '0' | '1' | '...' | '9';
```

## Tagging

vX.Y.Z - For normal

xX.Y.Z - For extra

git tag `tag`

git push origin `tag`
