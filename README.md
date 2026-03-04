# Haskllite

[![Compilation Status](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)](https://compiler-tester.insper-comp.com.br/svg/marcelopenas/haskllite)

## Run

    runghc main.hs [arguments]

## Diagrama sintático

![Diagrama Sintático](diagram.png)

## EBNF

```ebnf
EXPR = TERM, { ( '+' | '-' ) , TERM };

TERM = { FACTOR, ( '*' | '/' ) }, FACTOR;

FACTOR = "INT" | ("+" | "-"), FACTOR | "(", EXPR, ")";
```

## Tagging

vX.Y.Z - For normal

xX.Y.Z - For extra

git tag `tag`

git push origin `tag`
