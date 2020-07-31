(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.9
*)

fun car list = case list of (h::t) => SOME(h)
                          | _ => NONE;

fun cdr list = case list of (h::t) => SOME(t)
                          | _ => NONE;
cdr [1];
(* ここで、SOME []を返すけれど、nil もまたlistだからいいか *)
fun last list = case list of [x] => SOME(x)
                          | (h::t) => last t
                          | nil => NONE;

(* 筆者の解答: option型ではないから、修正が必要だったので私がやる *)
fun car (h::_) = SOME(h);

fun cdr (_::t) = SOME(t);

fun last [h] = SOME(h) | last (_::t) = last t;

fun car (h::_) = SOME(h)
  | car _ = NONE;

fun cdr (_::t) = SOME(t)
  | cdr _ = NONE;

fun last [h] = SOME(h) 
  | last (_::t) = last t
  | last _ = NONE;

(* ただ以下のようなWarningが出るのでMatch例外を呼び出すかWarningで甘んじるかはトレードオフかな。
stdIn:9.1-9.8 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
*)