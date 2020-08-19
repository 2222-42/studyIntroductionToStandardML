(* SML source file. Copyright (c) by 2222-42 2020.
* Q13.4
*)

(* signature ARRAY_2015 =
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
  end *)

signature SORT = sig
    val sort : 'a array * ('a * 'a -> order) -> unit
end

structure ArrayQuickSort : SORT = struct
    local open Array
    in 
        fun sort (array, comp) = 
            let 
                (* Q13.1 *)
                fun swap (i, j) = 
                    let
                        val valueOfI = Array.sub(array, i)
                        val valueOfJ = Array.sub(array, j)
                    in
                        (Array.update(array, i, valueOfJ); Array.update(array, j, valueOfI))
                    end
                fun twoSort array i j = 
                    case comp(sub(array, i), sub(array, j)) of
                        GREATER => swap (i, j)
                        | _ => ()
                fun threeSort array i =
                    case comp(sub(array, i), sub(array, i + 1)) of
                        GREATER => (case comp(sub(array, i),sub(array, i + 2)) of
                                    GREATER => ( case comp(sub(array, i+1),sub(array, i + 2)) of
                                            (* 3,2,1 *)
                                            GREATER => swap(i, i+2)
                                            (* 3,1,2 *)
                                          | _ => (swap(i, i+1); swap(i+1, i+2))
                                        )
                                  | _ => (swap(i, i+1))
                                            (* 2,1,3 *)
                                )
                      | _ => (case comp(sub(array, i), sub(array,i + 2)) of
                                (* 2,3,1 *)
                                GREATER => (swap(i, i + 2); swap (i+1, i+2))
                              | _ => 
                              (* twoSort array (i+1) (i+2) *)
                                ( case comp(sub(array, i+1),sub(array, i + 2)) of
                                    (* 1,3,2 *)
                                    GREATER => swap(i+1, i+2)
                                    (* 1,2,3 *)
                                  | _ => ()
                                )
                              )
                fun qsort (i, j) = 
                    if j <= i + 1 then ()
                    else if j = i + 2 then twoSort array i (i+1)
                    else if j = i + 3 then threeSort array i
                    else 
                        let 
                            (* Q13.2 *)
                            val pivot = 
                                let
                                    val d = (j - i) div 4
                                    val i1 = i + d
                                    val i2 = i + d * 2
                                    val i3 = i + d * 3
                                    val (position, value) = 
                                        if (j - i) < 30 then (i,sub(array,i))
                                        else
                                            case (comp(sub(array, i1), sub(array, i2))) of
                                                GREATER => 
                                                    (case (comp(sub(array, i2), sub(array, i3))) of
                                                        GREATER => (i2, sub(array, i2))
                                                    | _ => (case (comp(sub(array, i1), sub(array, i3))) of
                                                            GREATER => (i1, sub(array, i1))
                                                        | _ => (i3, sub(array, i3))
                                                        )
                                                    )
                                            | _ =>
                                                    (case (comp(sub(array, i2), sub(array, i3))) of
                                                        GREATER => (case (comp(sub(array, i1), sub(array, i3))) of
                                                            GREATER => (i1, sub(array, i1))
                                                        | _ => (i3, sub(array, i3))
                                                        )
                                                    | _ => (i2, sub(array, i2)) 
                                                    )
                                in
                                    (Array.update(array, position, sub(array, i));Array.update(array, i, value);value)
                                end
                            (* Q13.3 *)
                            fun partition(a,b) = 
                                if b < a then (a-1)
                                else
                                    let 
                                        fun scanRight a = 
                                        (* uncaught exception Subscript [subscript out of bounds] raised at: stdIn:70.55-70.58 *)
                                            if b < a then a
                                            else
                                            case comp(sub(array, a), pivot) of
                                                GREATER => a
                                              | _ => scanRight (a + 1)
                                        val a = scanRight a
                                        fun scanLeft b = 
                                            if b < a then b
                                            else
                                            case comp(sub(array, b), pivot) of
                                                GREATER => scanLeft (b - 1)
                                              | _ => b
                                        val b = scanLeft b
                                    in
                                        if b < a then (a - 1)
                                        else (swap(a,b); partition(a+1, b-1))
                                    end
                            val k = partition (i+1, j-1)
                            val _ = swap(i,k)
                        in
                            (qsort(i,k); qsort(k+1, j))
                        end
            in
                qsort(0, Array.length array)
            end
    end
end;

val target = Array.fromList [100,10,2,405,3,12,8,40,42];
ArrayQuickSort.sort(target, Int.compare);
(*
val it = () : unit
- target;
val it = [|2,12,40,3,10,8,42,100,405|] : int array
- target;
val it = [|2,3,8,10,12,40,42,100,405|] : int array
*)
val target1 = Array.fromList [100,10];
ArrayQuickSort.sort(target1, Int.compare);
target1;

val target2 = Array.fromList [10,100];
ArrayQuickSort.sort(target2, Int.compare);
target2;

val target123 = Array.fromList [1,2,3];
ArrayQuickSort.sort(target123, Int.compare);
val target132 = Array.fromList [1,3, 2];
ArrayQuickSort.sort(target132, Int.compare);
val target213 = Array.fromList [2,1,3];
ArrayQuickSort.sort(target213, Int.compare);
val target231 = Array.fromList [2,3,1];
ArrayQuickSort.sort(target231, Int.compare);
val target312 = Array.fromList [3,1,2];
ArrayQuickSort.sort(target312, Int.compare);
val target321 = Array.fromList [3,2,1];
ArrayQuickSort.sort(target321, Int.compare);

target123;
target132;
target213;
target231;
target312;
target321;

fun randomList (n) =
 let fun helper (n, s) theList =
  if n = 0
     then theList
     else
      let val min = 1
          val max = 75
          val nextInt = Random.randRange(min,max)
          val randomValue = nextInt s
       in helper (n - 1, s) (randomValue::theList)
      end;
 in helper (n, Random.rand(1, 1)) [] end;

val test1 = Array.fromList (randomList 20);
ArrayQuickSort.sort(test1, Int.compare);

val test2 = Array.fromList (randomList 50);
ArrayQuickSort.sort(test2, Int.compare);