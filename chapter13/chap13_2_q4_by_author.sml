signature SORT = sig
    val sort : 'a array * ('a * 'a -> order) -> unit
end

structure ArrayQuickSortOpt : SORT  =
   struct
   local
     open Array
   in
     fun sort (array,comp) =
         let
           fun swap (i,j) =
               let val temp = sub(array,i)
               in (update(array,i,sub(array,j)); update(array,j,temp))
               end
           fun sort3 i =
               case comp(sub(array,i),sub(array,i+1))
               of GREATER =>
                 (case comp(sub(array,i+1),sub(array,i+2))
                  of GREATER => (* 3 2 1 *)
                     swap(i,i+2)
                   | _ => (case comp(sub(array,i),sub(array,i+2))
                           of GREATER => (* 3 1 2 *)
                              let val ei = sub(array,i) in
                                (update(array,i,sub(array,i+1));
                                 update(array,i+1,sub(array,i+2));
                                 update(array,i+2,ei))
                              end
                            | _ => (* 2 1 3 *)
                              (swap(i,i+1))
                          )
                 )
                | _ =>
                  (case comp(sub(array,i+1),sub(array,i+2))
                    of GREATER =>
                       (case comp(sub(array,i),sub(array,i+2))
                         of GREATER => (* 2 3 1 *)
                            let val ei = sub(array,i) in
                              (update(array,i,sub(array,i+2));
                               update(array,i+2,sub(array,i+1));
                               update(array,i+1,ei))
                            end
                          | _ => (* 1 3 2 *)
                            (swap(i+1,i+2))
                       )
                     | _ =>  (* 1 2 3 *)
                       ())
           fun getPivot (i,j) =
               let
                 val delta = (j-i) div 4
                 val i = i + delta
                 val j = i + delta * 3
                 val mid = i + (j-i) div 2
                 val ie = sub(array,i)
                 val je = sub(array,j)
                 val me = sub(array,mid)
               in
                 case (comp(ie,me),comp(me, je))
                  of (LESS, LESS) => (mid,me)
                   | (LESS, _) => (case comp(ie, je) of LESS => (j,je) | _ => (i,ie))
                   | (_, GREATER) => (mid,me)
                   | _ => (case comp(ie, je) of LESS => (i,ie) | _ => (j,je))
               end
           fun qsort (i,j) =
               if j < i+2 then ()
               else if j = i+2 then case (comp(sub(array,i),sub(array,i+1)))
                                    of GREATER => swap(i,i+1)
                                     | _ => ()
               else if j = i + 3 then sort3  i
               else
                 let
                   val pivot = if (j-i) < 30 then sub(array,i)
                               else
                                 let val (pi,pivot) = getPivot(i,j-1)
                                 in update(array,pi,sub(array,i));
                                    update(array,i,pivot);
                                    pivot
                                 end
                   fun partition (a,b) =
                       if b < a then a
                       else
                         let
                           fun scanRight a =
                               if a > b then a
                               else
                                 case comp(sub(array,a),pivot)
                                 of GREATER => a
                                  | _ => scanRight (a+1)
                           val a = scanRight a
                           fun scanLeft b =
                               if b < a then b
                               else
                                 case comp(sub(array,b),pivot)
                                 of GREATER => scanLeft (b - 1)
                                  | _ => b
                           val b = scanLeft b
                         in
                           if b < a then a
                           else (swap(a,b);
                                 partition (a+1,b-1))
                         end
                   val a = partition (i+1,j-1)
                   val _ =  swap(i,a-1)
                 in
                   (qsort (i,a-1); qsort (a,j))
                 end
         in
           qsort (0,Array.length array)
         end
   end
   end;

val target = Array.fromList [100,10,2,405,3,12,8,40,42];
ArrayQuickSortOpt.sort(target, Int.compare);
(*
val it = () : unit
- target;
val it = [|2,12,40,3,10,8,42,100,405|] : int array
- target;
val it = [|2,3,8,10,12,40,42,100,405|] : int array
*)
val target1 = Array.fromList [100,10];
ArrayQuickSortOpt.sort(target1, Int.compare);
target1;

val target2 = Array.fromList [10,100];
ArrayQuickSortOpt.sort(target2, Int.compare);
target2;

val target123 = Array.fromList [1,2,3];
ArrayQuickSortOpt.sort(target123, Int.compare);
val target132 = Array.fromList [1,3, 2];
ArrayQuickSortOpt.sort(target132, Int.compare);
val target213 = Array.fromList [2,1,3];
ArrayQuickSortOpt.sort(target213, Int.compare);
val target231 = Array.fromList [2,3,1];
ArrayQuickSortOpt.sort(target231, Int.compare);
val target312 = Array.fromList [3,1,2];
ArrayQuickSortOpt.sort(target312, Int.compare);
val target321 = Array.fromList [3,2,1];
ArrayQuickSortOpt.sort(target321, Int.compare);

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
ArrayQuickSortOpt.sort(test1, Int.compare);

val test2 = Array.fromList (randomList 50);
ArrayQuickSortOpt.sort(test2, Int.compare);