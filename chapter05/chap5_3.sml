(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 5.3 パターンマッチングによるレコードの操作
*)

fun oldMalt {Brand, Distiller, Region, Age} = Age > 18;

val myMalt = {Brand = "Geln Moray", Distiller = "Glenlivet", Region = "the Highlands", Age = 28};

val {Brand = brand, Distiller = distiller, Age = age, Region = region} = myMalt;
val {Region = r, ...} = myMalt;
(* val r = "the Highlands" : string *)
val {Region, ...} = myMalt;
(* val Region = "the Highlands" : string *)

val distiller = (fn {Distiller,...} => Distiller) myMalt;

type malt = {Brand:string, Distiller:string, Region:string, Age:int};
fun getRegion ({Region, ...}: malt) = Region;