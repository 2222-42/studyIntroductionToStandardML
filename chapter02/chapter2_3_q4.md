問2.4

`fun sum n = if n = 0 then 0 else n + sum (n - 1);`

- Case1: 入力が0の場合は`sum 0`は0であり、`$S_0$`を計算する。
- Case2: 入力nが1以上の場合は、`sum (n -1)`が `$S_{n-1}$`を計算すると仮定する。すると、プログラムの定義より、`sum n`は`n + sum (n - 1)`であるから、`sum n`も`$S_n$`を計算する。
- IS: 以上より、`sum`は全ての自然数n にたいして、正しく`$S_n$`の値を計算する

`fun big_sum n = if n = 0 then 0 else sum(n) + big_sum(n - 1)`

- Case1: 入力が0の場合は`big_sum 0`は0であり、`$S_0$`を計算する。
- Case2: 入力nが1以上の場合は、`bigu_sum (n -1)`が `$S_{n-1}$`を計算すると仮定する。すると、プログラムの定義より、`big_sum n`は`sum(n) + big_sum (n - 1)`であるから、帰納法の仮定と前項の結果より、`big_sum n`も`$S_n$`を計算する。
- IS: 以上より、`big_sum`は全ての自然数n にたいして、正しく`$S_n$`の値を計算する
