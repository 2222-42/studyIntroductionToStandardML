(* SML source file. Copyright (c) by 2222-42 2020.
* Chap13.1
*)

(* 
教科書に記載されているシグネチャと型が違う箇所がある。
copy, copyVecには、`len`, `si`がなくなっている。
`appi` (int * 'a -> unit) -> 'a array -> unit
また、find, exists, fromVector, toVector, collate, toListなどの新たな関数の記載もある。
- signature Array = ARRAY;
[autoloading]
[autoloading done]
signature ARRAY_2015 =
  sig
    type'a array
    type'a vector
    val maxLen : int
    val array : int * 'a -> 'a array
    val fromList : 'a list -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val length : 'a array -> int
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val vector : 'a array -> 'a vector
    val copy : {di:int, dst:'a array, src:'a array} -> unit
    val copyVec : {di:int, dst:'a array, src:'a vector} -> unit
    val appi : (int * 'a -> unit) -> 'a array -> unit
    val app : ('a -> unit) -> 'a array -> unit
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify : ('a -> 'a) -> 'a array -> unit
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val findi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find : ('a -> bool) -> 'a array -> 'a option
    val exists : ('a -> bool) -> 'a array -> bool
    val all : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order) -> 'a array * 'a array -> order
    val toList : 'a array -> 'a list
    val fromVector : 'a vector -> 'a array
    val toVector : 'a array -> 'a vector
  end

疑問: こういったシグネチャのバージョン管理はどうなっているのだろうか。
*)

Array.maxLen;
val test1 = Array.array(3,0);
val test2 = Array.fromList([0,1,2]);
val test3 = Array.tabulate(3, fn x => x);
Array.length test3;
Array.sub(test2, 1);
Array.sub(test2, 3) handle exn => (print(exnName(exn)^"\n"); ~1);
Array.update(test3, 2, 4);
test3;
Array.update(test2, 3,5) handle exn => print(exnName(exn)^":"^exnMessage(exn)^"\n");

val source = Array.tabulate(5, fn x => x+1);
val target1 = Array.array(10,0);
Array.copy{src=source, dst=target1, di=2};

val sourceVec = Vector.tabulate(5, fn x => x+1);
val target2 = Array.array(10,0);
Array.copyVec{src=sourceVec, dst=target2, di=2};

Array.app(fn x => print(Int.toString(x)^" ")) target1;

Array.foldr (fn (h, R) => h + R) 0 target1;
Array.foldl (fn (h, R) => h::R) [] target2;

Array.modify (fn x => x*x) target2;
target2;

(*     val appi : (int * 'a -> unit) -> 'a array -> unit *)
Array.appi(fn (i,x) => print("position:"^Int.toString(i)^", value: "^ Int.toString(x)^"\n")) target1;
Array.modifyi (fn (i,x) => x-x+i) target2;
target2;
