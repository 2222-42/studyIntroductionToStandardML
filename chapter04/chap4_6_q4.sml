(* SML source file. Copyright (c) by 2222-42 2020.
* Q 4.4
*)

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

fun lower string = 
    let 
        val chr_list = explode string
        fun mapToLowerChrList list = map toLower list
    in
        implode (mapToLowerChrList chr_list)
    end;

(* 筆者の解答は以下の通り。回答者の解答でletを使っているが、それは必要なかった。 *)
fun lowerByAuthor s = implode (map toLower (explode s));

(* Strings.mapを使ったシンプルなケース *)
fun lowerInSimpleWay s = String.map toLower s

fun isLower c =
    let
        val i = ord(c)
    in
        (ord #"a" <= i) andalso (i <= ord #"z")
    end

fun toUpper c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isLower c then
            chr((ord c) - dif) 
        else c
    end;

fun upper string = 
    let 
        val chr_list = explode string
        fun mapToUpperChrList list = map toUpper list
    in
        implode (mapToUpperChrList chr_list)
    end;

(* 筆者の解答 *)
fun upperByAuthor s = implode (map toUpper (explode s));

(* Strings.mapを使ったシンプルなケース *)
fun upperInSimpleWay s = String.map toUpper s
