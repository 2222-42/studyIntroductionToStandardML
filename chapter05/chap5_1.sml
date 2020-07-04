(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 5.1 Record Type and Record Formula
*)

(* 
レコード：名前と式の組の集合
フィールド：名前の付いた式や型
ラベル：名前
*)

type malt = {Brand:string, Distiller:string, Region:string, Age:int};

val myMalt = {Brand = "Geln Moray", Distiller = "Glenlivet", Region = "the Highlands", Age = 28};

{Name = {FirstName="Kenny", LastName="Drew"}, Age = 72};

fun createGlen'sMale (name, age) = {Brand = name, Distiller = "Glenlivet", Region = "the Highlands", Age = age};
