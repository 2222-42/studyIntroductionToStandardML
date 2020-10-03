(* SML source file. Copyright (c) by 2222-42 2020.
* Q 4.5
*)

(* 問題文は
「第一引数で与えられた文字列が」「第二引数で与えられた文字列の」「先頭部分文字列になっているか否か」「を判定せよ」
問題の原因：引数の順序を間違えていた。

 *)
fun isPrefix string1 string2 =
    let 
        val size1 = size string1
        val size2 = size string2
    in
        if size2 < size1 then false
        else substring(string2, 0, size1) = string1
    end;

(* 筆者の解答 
筆者の解答はすごくシンプルだが、exceptionを吐くケースがあるのは注意
*)
fun isPrefixByAuthor s1 s2 = s1 = substring(s2, 0, size s1);

isPrefix "" "test";
isPrefix "t" "test";
(* expected: true
result: false
-> 引数の順序を間違えていた
*)
isPrefix "test2" "test";