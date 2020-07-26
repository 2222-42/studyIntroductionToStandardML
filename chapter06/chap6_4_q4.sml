(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.4
*)
fun null x = case x of nil => true
                    | (h::t) => false;

fun hd x = case x of (h::t) => h;

fun tl x = case x of (h::t) => t;
(*
- tl [];
stdIn:1.2-1.7 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
 uncaught exception Match [nonexhaustive match failure] 

exception Emptyを 明示的に出したい場合はどうすればいいのだろうか？
*)

(* 筆者の解答 (不要なものはちゃんと_してるからそれを私も習慣づけよう)*)
fun nullByAuthor nil = true
  | nullByAuthor (h::_) = false;

fun hdByAuthor (h::_) = h;

fun tlByAuthor (_::t) = t;
