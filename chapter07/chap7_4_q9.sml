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