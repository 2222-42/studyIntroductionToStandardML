
# 1.4 変数の束縛と識別子

## 問1.5

英字および`'`で始まるという条件、もしくは特定の特殊記号の列からということなので、
以下の文字列は変数は使えない

- 1stLady
- __
- _abc
- 123
- %1
- ##'
- ()

なお、`val 123 = ...`という宣言では、"binding not exhaustive"というWarningが表示されて、値の束縛ができない。一方で、`val 1stLady = ...`という宣言では"non-constructor applied to argument in pattern"というエラーが生じる。

なお、`()`は、変数の識別子の条件を満たさないうえに、予約語の組み合わせであるため、次のようなエラーが表示される:
```
Error: pattern and expression in val dec do not agree [tycon mismatch]
    pattern:    unit
    expression:    real
    in declaration:  
...
```
。

`'`から始まる識別子は型変数としてのみ使用可能だから値を表す変数としては使用できない

- 'a

英字から始まったあと、英字、数字、'、_のいずれかなる文字列なので、以下は使えない

- ten%

以下は予約語のため、使用できない

- and
- +

使えるのは以下の文字列

- name1
- ++
