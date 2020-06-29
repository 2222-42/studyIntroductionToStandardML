(* SML source file. Copyright (c) by 2222-42 2020.
* 
*)

val emptySet = fn x => false;
(* val emptySet = fn : 'a -> bool *)
fun singleton n = fn x => x = n;
(* val singleton = fn : ''a -> ''a -> bool *)
fun insert S n = fn x => S x orelse x = n;
(* val insert = fn : (''a -> bool) -> ''a -> ''a -> bool *)
fun member S n = S n;
(* val member = fn : ('a -> 'b) -> 'a -> 'b *)
fun union S1 S2 = fn x => S1 x orelse S2 x;
(* val union = fn : ('a -> bool) -> ('a -> bool) -> 'a -> bool *)
fun intersection S1 S2 = fn x => S1 x andalso S2 x;
(* val intersection = fn : ('a -> bool) -> ('a -> bool) -> 'a -> bool *)
fun difference S1 S2 = fn x => S1 x andalso not (S2 x);
(* val difference = fn : ('a -> bool) -> ('a -> bool) -> 'a -> bool *)

singleton ("2222-42", "Kuroku Shijuuni");
(* val it = fn : string * string -> bool *)
val InsertedName = insert Name ("testID", "tester");
(* val InsertedName = fn : string * string -> bool *)
(* 
- insert Name 1;
stdIn:1.2-1.15 Error: operator and operand do not agree [overload conflict]
  operator domain: string * string
  operand:         [int ty]
  in expression:
    (insert Name) 1
*)

member Name ("testID", "tester");
member InsertedName ("testID", "tester");
val IntersectedNames = intersection InsertedName Name;

member IntersectedNames ("2222-42", "tester");
member IntersectedNames ("testID", "tester");

val SetOfIdAndName = singleton (1, "test1");
(* member SetOfIdAndName 1 *)
member SetOfIdAndName (1, "test2");
member SetOfIdAndName (1, "test1");
(* val InsertedUsers = insert SetOfIdAndName ("testID", "tester"); *)
val InsertedUsers = insert SetOfIdAndName (2, "test2");
