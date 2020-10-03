(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.1
*)

(* 
複雑なデータ処理を行うプログラムを開発するために、
システム定義のデータ構造以外に
ユーザーによる新しいデータ構造を定義する機構が必要

MLではデータ構造の定義はデータ型の宣言を通じて行われる。
-> 2分木を例にして説明する
*)

(* 
木のノードが持つ情報の型をaとすると、2分木は以下のように再帰的に定義される。

1. 空の木は2分木である
2. a型のデータvと、2つの2分木T1, T2からなる組(v, T1, T2)は2分木である

つまり、2分木は以下のように再帰的に定義された集合
```
tree(a) = {Empty} \cup (a \times tree(a) \times tree(a))
```
の要素とみなせる。

型を、その型を持つデータの集合と考えると、上の等式は2分木の型の定義とみなすことができる。
*)

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

(*
'a tree は新しい多相型構成子
EmptyとNodeはデータ構成子
- Emptyのように引数を持たないデータ構成子は、定義されたデータ型をもつ定数として使用できる
- Nodeのように引数を持つデータ構成子は、その構成子の引数の型から、定義されたデータ型への関数と同様に使用できる
*)

Node ("a", Node("b", Empty, Empty),
           Node("c", Node("d", Empty, Empty), Empty));

(* 
例：
文字列にコード化された2分木をstring tree型データに変換する処理

pre-orderによる木の表記法

- 空の木Empty は、空文字列を表す
- Node(a, L, R)
  - L(左部分木) -> S_L
  - R(右部分木) -> S_R
  - a(S_L)(S_R)

これによって得られた文字列表現をpre-order表現と呼ぶ。
*)

(* 
文字列表現から2分木を構築する関数fromPreOrder

pre-order 表現と対応する二分木の関係の再帰的記述
1. 空文字列はEmptyに対応する
2. 空文字列でないpre-order表現は、r(S_l)(S_r)の形をしている。さらに、S_lとS_rもpre-order 表現である。
   S_lとS_rに対応する2分木をT_l、T_rとすると、二分木はNode(r, T_l, T_r)となる
*)

fun fromPreOrder s =
  let fun decompose s = 
      let 
        fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1)
        fun searchRP s p n = 
          case (substring(s, p, 1), n) 
          of (")", 0) => p
          | (")", n) => searchRP s (p+1) (n-1)
          | ("(", n) => searchRP s (p+1) (n + 1)
          | (_, n) => searchRP s (p + 1) n
        val lp1 = searchLP s 0
        val rp1 = searchRP s (lp1 + 1) 0
        val lp2 = searchLP s (rp1 + 1)
        val rp2 = searchRP s (lp2 + 1) 0
      in
        (
          substring (s, 0, lp1),
          substring (s, lp1 + 1, rp1-lp1-1),
          substring (s, lp2 + 1, rp2-lp2-1)
        )
      end
  in if s = "" then Empty
     else let val (root, left, right) = decompose s
          in Node(root, fromPreOrder left, fromPreOrder right)
          end
  end;

(* 
種々のデータ型を `datatype` 文を使って定義することができる。

構文：
  datatype typeSpec = 
            | Con_1 <of type1>
            | Con_2 <of type2>
            ...
            | Con_n <of typen>

typeSpecは定義する型の記述
  型パラメーターを含まない単層型の場合はその型の名前
    tid tycon の形で指定
      tid は 'a などの型変数名
  多相型の場合は
    (tid, ..., tid) tycon の形で指定

型の構造の定義は、可能な構造を `|` で区切って列挙する

`Con_i` はデータ構成子と呼ばれる識別子

`of type_i` はデータ構成子が持つ内部構造の記述
  内部構造がなければ省略する
*)
