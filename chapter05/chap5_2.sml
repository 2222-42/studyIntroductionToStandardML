(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 5.2
*)

(* フィールドの値取り出しを行うための組み込み関数`#l` *)

val myMalt = {Brand = "Geln Moray", Distiller = "Glenlivet", Region = "the Highlands", Age = 28};

#Distiller myMalt;

fun oldMalt (x: {Brand:'a, Distiller:'b, Region:'c, Age: int}) = #Age x > 18;