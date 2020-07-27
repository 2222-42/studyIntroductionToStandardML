# 問 6.8

## map

map (fn x => ...) L

リストLがnilなら`[]`を返す。つまり、`Z = []`。

リストLがh::tの形なら、部分リストtに対する値Rを計算する。

hとRに特定の演算fを適用し、得られた結果を返す
- h: 'a
- R: 'a list

演算fは
- `f h R = h::R` : 'a -> 'a list -> 'a list

### 筆者の解答からの補足

`f = fn (h, R) => f h :: R`
とfn式にした方がよい

## @

L1 @ L2

リストL1がnilなら特定の値L2を返す。つまり、`Z = L2`。

リストL1がh::tの形なら、部分リストtに対する値Rを計算する。(t@L2を計算する)

hとRに特定の演算fを適用し、得られた結果を返す
- h: 'a
- R: 'a list

演算fは
- `f h R = h :: R` : 'a -> 'a list -> 'a list

### 筆者の解答からの補足

`f = fn (h, R) => h :: R`
とfn式にした方がよい

## flatten

flatten L

リストLがnilなら特定の値`[]`を返す。つまり、`Z = []`。

リストLがh::tの形なら、部分リストtに対する値Rを計算する。(flatten tを計算する)

hとRに特定の演算fを適用し、得られた結果を返す
- h: 'a list
- R: 'a list

演算fで配列同士を結合する。
- `f h R = h @ R` : 'a list -> 'a list -> 'a list

### 筆者の解答からの補足

`f = fn (h, R) => h @ R`
とfn式にした方がよい

## filter

filter P L

リストLがnilなら特定の値`[]`を返す。つまり、`Z = []`。

リストLがh::tの形なら、部分リストtに対する値Rを計算する。(filter P tを計算する)

hとRに特定の演算fを適用し、得られた結果を返す
- h: 'a
- R: 'a list

`R`はtをfilterした結果なので。

演算fで先頭の要素hについてPを満たすかどうかを確認し、満たすなら、Rの先頭要素に追加する
- `f h t = if (P h) then h::R else R` : 'a -> 'a list -> 'a list

### 筆者の解答からの補足

`f = fn (h, R) => if (P h) then h::R else filter R`
とfn式にした方がよい

## permutations

permutations L

リストLがnilなら特定の値`[]`を返す。つまり、`Z = []`。

リストLがh::tの形なら、
- tがnilの時は`[[x]]`を返し
- そのほかの場合は、部分リストtに対する値Rを計算する。(permutations tを計算する)

hとRに特定の演算fを適用し、得られた結果を返す

```
insert s (h::t) = 
               let val L = insert s t
               in (s::(h::t)) :: (map (fn x => h::x)L)
               end
```

演算fで先頭の要素hを、Rのリストのリストの各要素の先頭に追加していく。
- `f h R = flatten (map (fn x => insert h x) R)`: 'a -> 'a list list -> 'a list list

### 筆者の解答からの補足

`f = fn (h, R) => foldr (fn (a, b) => insert h a @ b) nil R`
とfoldrを使って、また、fn式にした方がよい

foldrの使い方を学ぶ前にfoldr導入するのはどうなん？となってしまうが、まぁ、良いだろう。
