# 6 リスト

## 6.1 リスト構造


リスト：0個以上の要素の列

実行時に拡張が可能な動的なデータ構造

-> 動的なデータ構造なので、その実装の構造を理解しておけば、効率的なプログラムを設計するのに役立つ


- L: pointer -> [v1, p1]
- p1: -> [v2, p2]
- p2: -> [v3, p3]
- ...
- p(n-1) -> [vn, nil]

1. リストはその大きさにかかわらず1つのポインタで表現される
2. 空のリストはnilと呼ばれる特殊なポインタで表現される
3. 各要素は、先頭から順にポインタを辿ればアクセスできる
4. リストから先頭の値を取り除いても、リストに要素を付け加えたものもリスト

こういった特徴、特に最後の性質によって、リストは関数型言語において、実行時に生成される種々のデータを蓄積し加工する際のデータ構造
として使用される


### Q6.1 

集合Aの要素からなる全てのリストの集合は、集合に関する以下のような方程式の解と考えることができる

`L = Nil \cup (A \times L)`

この方程式の再紹介を以下の手順で求めよ

#### 1

集合の系列`X_i`を以下のように定める

- `X_0 = Nil`
- `X_{i+1} = X_i \cup (A \times X_i)`

もし`L`に関する方程式が解を持てば、任意の`i`に対して、

`X_i \subseteq L`

であることを`i`に関する帰納法で示せ

(Case0) `i = 0`のとき、`X_i = Nil`であり、`L = Nil \cup (A \times L)`であるから、`X_0 \subseteq L`となる。

(Case n) `i = n-1` のとき、`X_{n-1} \subseteq L`と仮定する。

`X_n =  X_{n-1} \cup (A \times X_{n-1})`なので、`X_{n-1} \subseteq L` かつ `A \times X_{n-1} \subseteq L`であることを示せば十分である。

`X_{n-1} \subseteq L`は帰納法の仮定より明らか。

`A \times X_{n-1} \subseteq L`については、帰納法の仮定より`X_{n-1} \subseteq L`であり、`L`の条件の`L = Nil \cup (A \times L)`より、`A \times X_{n-1}`により、`L`に含まれることがわかる。

よって、`X_n \subseteq L`となる。

Case nに関する筆者の解答:

- `X_n =  X_{n-1} \cup (A \times X_{n-1})`
- `X_{n-1} \subseteq L` (帰納法の仮定より)
- `X_{n-1} \cup (A \times X_{n-1}) \subseteq L \cup (A \times L)`
- `L \cup (A \times L) \subseteq L`

よって、`X_n \subseteq L`。

#### 2

この系列を用いて、集合Xを以下のように定義する。

`X = \bigcup_{i >= 0} X_i`

このとき、Xは上記の方程式を満たすことを示し、したがって、Xが上記方程式の再紹介であることを確認せよ。

1より、任意の`i` について `X_i \subseteq L`であるから、`X \subseteq L`である。

`X`より小さく、かつ、上記方程式を満たす集合`Y`があると仮定する。

`e`を`X`に含まれ、`Y`に含まれない要素とする。(`e \in X \ Y`)

`X`の任意の要素は`L`に含まれるので、`e`は`Y`に含まれないことになり、`Y`は上記方程式を満たさない集合となる。

よって、`X`は最小解である。

筆者の解答

`X = \bigcup_{i >= 0} X_i`である。

- `X \cup (A \times X) = X \cup A \times (\bigcup_{i >= 0} X_i)`
- `X \cup A \times (\bigcup_{i >= 0} X_i) = X \cup (\bigcup_{i >= 0} A \times X_i)`
- `X \cup (\bigcup_{i >= 0} A \times X_i) = X_0 \cup X \cup (\bigcup_{i >= 0} A \times X_i)` (`A \cup (A \cup B )` は `A \cup B` と等しいから)
- `X_0 \cup X \cup (\bigcup_{i >= 0} A \times X_i) = X_0 \cup (\bigcup_{i >= 0} X_i \cup (A \times X_i))`  (`X = \bigcup_{i >= 0} X_i`であるから)
- `X_0 \cup (\bigcup_{i >= 0} X_i \cup (A \times X_i)) = X_0 \bigcup_{i >= 1} X_i ` (1より、`X_{i+1} = X_i \cup (A \times X_i)`であるから)
- `X_0 \bigcup_{i >= 1} X_i = \bigcup_{i >= 0} X_i`

よって、Xは方程式の解である。一方、1の結果から、Xは任意の解に含まれるため、方程式の最小な解である。
