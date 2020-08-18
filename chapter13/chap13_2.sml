(* SML source file. Copyright (c) by 2222-42 2020.
* Chap13.2 Q13.1 Q13.2
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
                fun qsort (i, j) = 
                    if j <= i + 1 then ()
                    else 
                        let 
                            (* Q13.2 *)
                            val pivot = 
                                let
                                    val pivot = sub(array,(i+j) div 2)
                                in
                                    (Array.update(array, (i+j) div 2, sub(array, i));Array.update(array, i, pivot);pivot)
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