(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.5
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

fun isAlpha c = 
    let 
        val i = ord(c)
    in 
        ((ord #"a" <= i) andalso (i <= ord #"z")) orelse ((ord #"A" <= i) andalso (i <= ord #"Z"))
    end;

fun isNum c = 
    let 
        val i = ord(c)
    in 
        (ord #"0" <= i) andalso (i <= ord #"9")
    end;

fun isAlphaNumeric c = 
    (isAlpha c) orelse (isNum c);

fun isLower c =
    let
        val i = ord(c)
    in
        (ord #"a" <= i) andalso (i <= ord #"z")
    end

fun isUpper c =
    let val i = ord(c)
    in (ord #"A" <= i) andalso (i <= ord #"Z")
    end;

fun toLower c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isUpper c then
            chr((ord c) + dif) 
        else c
    end;

fun toUpper c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isLower c then
            chr((ord c) - dif) 
        else c
    end;
