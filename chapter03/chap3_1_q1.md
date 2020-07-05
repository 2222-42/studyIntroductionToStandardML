
問3.1

> `3 * 3.14;`

infix `*` は左と右とが型を一致していないといけない。

> `fun f1 x = (x 1, x true);`

右のbodyの中を見ると、仮引数`x`は関数であり、それの入力の型がそれぞれ、`x 1`ではintであり、`x true`ではbool であり、型エラーを起こす。

> `fun f2 x y = if true then x y else y x;`

`x y`を見ると、`x: 'a -> 'b` という関数になり、`y: 'a`になるが、
`y x`を見ると、`y: 'a -> 'b` という関数になり、`x: 'a`となる。

型の定義が再帰的に循環することになるため、壊れる。

> `fun f3 x = x x;`

`x x`を見ると、`x: 'a -> 'b` という関数になり、`x: 'a`になる。

型の定義が再帰的に循環することになるため、壊れる。