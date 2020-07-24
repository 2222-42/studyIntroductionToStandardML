(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.14
*)

(* 各式について型が正しいかを判定し、正しければ結果の型を予測せよ *)

(* "M" = #"M"; *)
(* 
expect1: 左の `"M"` はstringであり、 `#"M"` は char であるから、型が正しくない。
*)

"ML" = "SML";
(* 
expect1: 両方ともにstringであるから、型は正しい。
expect2: false
*)

fn x => (x, x) = (x, x);
(* 
expect1: xの型が不明であり、eqtypeかどうかわからないから、型が正しくない
result1: 型が正しい。

result2: 
- fn x => (x, x) = (x, x);
stdIn:56.16 Warning: calling polyEqual
val it = fn : ''a -> bool

ただし、xに関数を入れるとエラーが起きる
- (fn x => (x, x) = (x, x))(fn f x => x); 
stdIn:59.30-59.33 Error: non-constructor applied to argument in pattern: f
stdIn:59.37 Error: unbound variable or constructor: x
stdIn:59.1-59.39 Error: operator and operand do not agree [equality type required]
  operator domain: ''Z
  operand:         'Y -> _
  in expression:
    (fn x => (<exp>,<exp>) = (<exp>,<exp>)) (fn _ => <errorvar>)

推測であるが、本体のexpを評価するときに`(x, x) = (x, x)`より、eqtype構成子の中身の型もまたeqtypeであるとMLが型推論したのであろう
*)

(* fn (x, f) => (f (fn x => x), f x, x = x) *)
(* 
expect1: 型が正しくない
f (fn x => x)より、fの引数は関数であり、
f xより、xは関数になり、
よって、x = xで、eqtype構成子ではない`->`が含まれることになる。
筆者の回答: x = xは関数同士の比較となり，型エラーとなる．

result1:
stdIn:60.10-60.37 Error: operator and operand do not agree [equality type required]
  operator domain: ''Z * ''Z
  operand:         ('Y -> 'Y) * ('Y -> 'Y)
  in expression:
    x = x
*)