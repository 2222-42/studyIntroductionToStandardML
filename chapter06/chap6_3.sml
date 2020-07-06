(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 6.3 パターンマッチングによるリストの分解
*)

(*
リストに対する基本演算: 
- 空か否かを検査する操作と
- 空でないリストをその先頭要素と残りのリストに分解する操作

```
case L of nil => E1
        | (h::t) E2(h, t)
```

リストのための場合分け構文の一般的なもの:
```
    case exp of pat_1 => exp_1 | pat_2 => exp_2 | ... | pat_n => exp_n
```

- pat_iは処理対象となるリストの構造を指定するためのパターン
- exp_iはそのパターンにマッチしたときに行う処理の記述

リスト構造は以下と、変数や組パターンなどの他のパターンと組み合わせて記述する:
- 空リスト `nil`
- リスト構成パターン `pat_1::pat_2`
- 有限リストパターン `[pat_1, ..., pat_n]`
- 匿名パターン `_`

パターンは上から順にチェックされ、最初にマッチするパターンに対応する定義が選択され、実行される

各パターンは排反でなくともよく、全ての場合を尽くさなくてもよい。
-> 定義されていない構造を持つ式を評価しようとするとMatch例外が発生する。
-> コンパイラは全ての場合を尽くしていない場合に "Warning: match nonexhaustive"と警告メッセージを表示する

慣習：
- 起こりえないパターンに対しては、エラー処理コードを書き、
- case文のパターンは全ての場合を尽くすようにするのが習慣。

fun 構文や関数式に対しては、場合分け構文を統合した記法が定義されている。
(StandardMLの文法の定義では、パターンを含んだfn構文が基本構文とされ、
case 構文はパターンを含んだfn式の適用構文の略記法とみなされている)

*)

case nil of nil => 0
          | (h::t) => h;

case [1, 2] of nil => 0
            | (h::t) => h;

fun length L = case L of nil => 0
                        | (h::t) => 1 + length t;

(* `zip nil`とかすると、Warningが出る。 *)
fun zip x = case x of (h1::t1, h2::t2) => (h1, h2)::zip(t1, t2)
                | _ => nil;

fun unzip x = case x of (h1, h2):: t =>
                    let val (L1, L2) = unzip t
                    in (h1::L1, h2::L2)
                    end
                   | _ => (nil,nil);

zip ([1,2,3], [4,5,6]); 
unzip it;
val result2 = zip ([1,2,3], [4,5,6,8]);
unzip result2;
(* 名前付けずにitとかで読み込もうとすると実行時エラーになるのはなんでだろう
-> 関数の後にセミコロンを付けていなかったのが原因
*)
zip ([1,2,3], [4,5,6,8]);
unzip it;

fun last L = case L of [x] => x
                    | (h::t) => last t;
(*  Warning: match nonexhaustive *)

(* 
- last nil;
stdIn:2.1-2.9 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)

uncaught exception Match [nonexhaustive match failure]
*)

fun lengthF nil = 0
    | lengthF (h::t) = 1 + lengthF t