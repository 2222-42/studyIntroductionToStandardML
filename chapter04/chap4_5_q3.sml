(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.5 and Q4.3
*)

(* eqtype char *)

(* 
一文字データの型
0~255
0~127はASCII表現と一致

範囲を超えたらexceptionが出る
*)

#"c";
#"\a";
#"\v";

(* char型データ操作のための例外と基本演算 
- exception Chr
- chr
- ord
- str
- >
- >=
- <
- <=
*)

fun isSpace c = 
    let 
        val i = ord(c)
    in 
        not((ord #"a" <= i) andalso (i <= ord #"z")) andalso not((ord #"A" <= i) andalso (i <= ord #"Z")) andalso not((ord #"0" <= i) andalso (i <= ord #"9"))
    end;

fun isSpaceByAuthor c = 
    case c of
         #" " => true
       | #"\t" => true
       | #"\n" => true
       | #"\v" => true
       | #"\f" => true
       | _ => false;

fun isAlpha c = 
    let 
        val i = ord(c)
    in 
        ((ord #"a" <= i) andalso (i <= ord #"z")) orelse ((ord #"A" <= i) andalso (i <= ord #"Z"))
    end;

(* 「char 型の比較は、その内部表現の数字の大小関係で行われる」(p. 66)
だから、筆者のようにchar型の比較演算子を使えばよく、回答者の解答ではそれをやっていないだけ。
*)
fun isAlphaByAuthor c = c >= #"a" andalso c <= #"z" orelse c >= #"A" andalso c <= #"Z";

fun isNum c = 
    let 
        val i = ord(c)
    in 
        (ord #"0" <= i) andalso (i <= ord #"9")
    end;

fun isNumByAuthor c = c >= #"0" andalso c <= #"9";

fun isAlphaNumeric c = 
    (isAlpha c) orelse (isNum c);

fun isAlphaNumericByAuthor c = isAlphaByAuthor c orelse isNumByAuthor c;

fun isLower c =
    let
        val i = ord(c)
    in
        (ord #"a" <= i) andalso (i <= ord #"z")
    end;

fun isLowerByAuthor c = #"a" <= c andalso c <= #"z";

fun isUpper c =
    let val i = ord(c)
    in (ord #"A" <= i) andalso (i <= ord #"Z")
    end;

fun isUpperByAuthor c = #"A" <= c andalso c <= #"Z"

fun toLower c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isUpper c then
            chr((ord c) + dif) 
        else c
    end;

fun toLowerByAuthor c =
    if isUpperByAuthor c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;

fun toUpper c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isLower c then
            chr((ord c) - dif) 
        else c
    end;

fun toUpperByAuthor c =
    if isLower c
    then chr (ord #"A" + (ord c - ord #"a"))
    else c;
