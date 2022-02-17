
# 循環2重リンクリスト

本発表では、循環2重リンクリストに関して、その定義と実装方法について扱う。

なお、実装や説明には、Standard MLおよびSML#を利用する。

# 循環2重リンクリストの前準備

循環2重リンクリストをStandard MLで実装するにあたって、前準備として参照型と評価順序について説明する。なお、Standard MLの文法についての説明は省略する。

## 参照型について

MLの変数は、その変数に束縛された値を表す式である。そのため、変数の値を「更新する」といった概念はない。しかし、処理するデータ構造によっては、共有変数の更新ができると、効率よいプログラムが書ける場合が多い。この目的のために、特殊なデータ型である `ref` 型構成子が用意されている。

`t ref`型: 
- `t`型のデータへのポイント
- MLではこの型を`t`の参照型と呼ぶ
- `t`の型に関わらず、同一のポイントか否かの等値性テストが可能な`eqtype`である

以下が、`ref`型に関する宣言である。

```sml
infix 3 :=
val ref : 'a -> 'a ref
    (* 型構成子でありながら、ref型データのデータ構成子でもある *)
val ! : 'a ref -> 'a
    (* 値を取り出す *)
val := : 'a ref * 'a -> unit
    (* 参照型データの値を更新する演算子 *)
```

## 評価順序とそのための構文　

参照型では、評価の順序によって結果が異なる。

式の評価順序: 
- 値の定義構文、`let ... in exp end`では、順々に評価
- 組やレコード式などの同一レベルの式が並んでいる場合は、左から順に評価
   - レコード式の評価の結果はラベルによってソートされるが、評価順序は表記された式の順序
- 関数適用(`exp1 exp2`)の評価は、
   1. `exp1`を評価し、
   2. 関数式`fn x => exp0`を得、
   3. `exp2`を評価し、値vを得
   4. 最後に`x`を`v`に束縛し、`exp0`を評価する

評価順序の制御のための構文: 
- `(exp1; ...; expn)`
  - `exp1`から`expn`をこの順に評価し、 `expn`の値をこの式全体の値とする
- `exp1 before exp2`
  - `exp1`, `exp2`の順で評価し、`exp1`をこの式全体の値とする
  - ただし、`exp2`は`unit`型の式でなければならない
    - `before`だから
- `while exp1 do exp2`
  - `bool`型を持つ`exp1`の評価結果が`false`になるまで、`exp1`と`exp2`をこの順に繰り返し評価
  - 結果は常に`()`である

### 例

以下の式の評価の結果を予測せよ。

```sml
(fn f => (print "a\n"; f))
(fn x => (print "b\n"; x))
(print "c\n"; 1);
```

```sml
(fn f => (print "a\n"; f))
(
    (fn x => (print "b\n"; x))
    (print "c\n"; 1)
);
```

# 循環2重リンクリスト

さて、循環2重リンクリストについて、説明する。

## 循環2重リンクリストの説明

循環2重リンクリスト(双方向連結リスト)とは、通常のリストとは異なる点が2点ある: 

1. 各要素は、後続の要素へのポインタだけではなく、前の要素へのポインタを有し(2重リンクリスト)、
2. 先頭の要素は末尾の要素へのポインタを有し、末尾の要素は先頭の要素へのポインタを有する(循環リスト)。

通常のリストは、先頭要素の取り出しおよび要素を順に処理するのに適したデータ構造であるが、一方、循環2重リンクリストは、リストを逆方向にたどったり、リストの中間要素を取り除くといった操作を効率的に行える柔軟なリスト構造である。実際、リスト全体を反復処理するケースでは、循環2重リンクリストの循環性が柔軟性を提供してくれる。(cf: [Linuxカーネルが循環的な二重リンクリストを使用してプロセスのリストを格納するのはなぜですか?](https://jpcodeqa.com/q/e0e9ed377dc908012d58df7b14dbc857))

## SMLでの定義と種々の関数の実装

次に、Standard MLにおける実装を見よう。

```sml
(* 参照型とデータ型定義を組み合わせて循環2重リンクリストを作る *)
datatype 'a cell = NIL | CELL of {data: 'a, left: 'a cell ref, right: 'a cell ref}
type 'a dlist = 'a cell ref;
```

通常のリストと異なり、`data`や後続(`right`)のcellへの参照だけではなく、前方(`left`)への参照がある。特に、末尾のcellは先頭のcellへの参照を持ち、また、先頭のcellは末尾のcellへの参照を持っている。

循環2重リンクリストの基本操作関数の宣言は以下の通りである。

```smi
dataDlist : 'a dlist -> 'a (* 先頭のデータを取り出す *)
rightDlist : 'a dlist -> 'a dlist (*ポインタを右にたどる*)
leftDlist : 'a dlist -> 'a dlist (*ポインタを左にたどる*)
insertDlist : 'a -> 'a dlist -> unit (*要素を追加する*)
singleDlist : 'a -> 'a dlist (*要素1つの循環2重リンクリストを作る*)
deleteDlist : 'a dlist -> unit (*先頭要素を削除する*)
fromListToDlist : 'a list -> 'a dlist (*リストを循環2重リンクリストに変換する*)
```

### 連結させる操作から循環2重リンクリストを眺める

2つの循環2重リンクリストを連結させる関数を考えよう。つまり、

- `dlist1`: `[l1 | d1 | r1 as ref (CELL{right=r11}, ...) ]`
- `dlist2`: `[l2 | d2 | r2 as ref (CELL{right=r21}, ...) ]`

とあった場合に、

- `dlist1`: `[l2 | d1 | r1 as ref (CELL{right=r21}, ...) ]`
- `dlist2`: `[l1 | d2 | r2 as ref (CELL{right=r11}, ...) ]`

となるような関数である。

以下がその宣言と実装である。

```smi
val concatDlist : 'a dlist -> 'a dlist -> unit (* 2つの循環2重リンクリストを連結する関数 *)
```

```sml
fun concatDlist dlist1 dlist2 =
    case (dlist1, dlist2) of
            (ref NIL, _) => (dlist1 := !dlist2)
          | (_, ref NIL) => (dlist2 := !dlist1)
          | (d1 as ref (CELL {right=r1 as ref (CELL{right=r11,...}), left=l1, ...}),
            d2 as ref (CELL {right=r2 as ref (CELL{right=r21,...}), left=l2, ...}))
                => let
                    val previousL1 = !l1
                    val previousR11 = !r11
                   in
                    (l1 := !l2; l2 := previousL1; r11 := !r21; r21 := previousR11)
                   end;
```

- `left`の更新は
  - `dlist1`の`left`の値を、`dlist2`のleftの値、つまり、`dlist2`の末尾の要素への参照にし、
  - `dlist2`の`left`の値を、更新前の`dlist1`のleftの値、つまり、更新前の`dlist1`の末尾の要素への参照にし、
- `right`の更新に関しては、`right`の`CELL`の`left`は更新しないようにすることに気をつけ、`left`の場合と同様に行う。


### 循環2重リンクリストをリストにする操作にするところから見る、大変さ

循環2重リンクリストは循環するため、通常のリストのように空になるまで再帰的に処理を行うといったプログラミは書けない。そのため、処理を行ったセルを記録し、すでに処理済みのセルが見つかるまで処理を行うようなコードを書かなければならない。

以下が求めている関数の宣言である。

```smi
val dlistToList : 'a dlist -> 'a list
```

この関数の適切な定義のためには、以下の2つが必要である。

1. 循環構造を辿る際の終了条件の適切な判定
2. `left`および`right`フィールドをたどると自分自身に戻ってくる `CELL`への参照型データの作成

実装は次のようになる。

```sml
```

# 循環2重リンクリストの長所と短所

GeeksforGeeksの記事から引用し、翻訳する[Doubly Circular Linked List | Set 1 (Introduction and Insertion)](https://www.geeksforgeeks.org/doubly-circular-linked-list-set-1-introduction-and-insertion/)

## 長所

- このリストは、両方向、つまり、先頭から末尾へ、末尾から先頭へとトラバースできる。
- 先頭から末尾へ、また、末尾から先頭への移動が、定数時間`O(1)`で済む。
- 循環2重リンクリストは、フィボナッチヒープなどの高度なデータ構造の実装にも使われている。

(フィボナッチヒープについては知らないので調べる)

## 短所

- それぞれのノードが前のポインタを収容するために、わずかながら追加のメモリを要する。
- リストの実装や操作において、多くのポインタが関与する。そのため、ポインタは注意深く扱われねばならない。さもなくば、そのリストのデータは失われる。


# 参考文献

- 大堀淳(2021)『プログラミング言語 Standard ML入門』共立出版株式会社
- 大堀淳、上野雄大(2021)『SML#で始める実践MLプログラミング』共立出版株式会社
- [Doubly Circular Linked List | Set 1 (Introduction and Insertion)](https://www.geeksforgeeks.org/doubly-circular-linked-list-set-1-introduction-and-insertion/)
