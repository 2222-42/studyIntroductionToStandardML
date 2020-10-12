signature ENV =
sig
  type env
  val emptyEnv : unit -> env
  val bind : string * Types.value * env -> unit
  val lookUp : string -> env -> Types.value
  val domain : env -> string list
end

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

structure Env : ENV = 
struct
  type env = (string list * (string * Types.value) tree) ref
  fun emptyEnv () = (ref (nil, Empty) : env)
  fun enter (key:string, v, env) = 
    case env of 
        (keyList, Empty) => 
              (keyList @ [key], Node((key, v), Empty, Empty))
        | (keyList, Node((key', v'), L, R)) =>
            if key = key' then 
              (keyList, Node((key', v'), L, R))
            else if key > key' then 
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, R))
              in
                (newKeyList, Node((key', v'), L, newTree))
              end
            else 
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, L))
              in
                (newKeyList, Node((key', v'), newTree, R))
              end

  fun bind (key: string, v:Types.value, env) = 
    env := enter(key, v, !env)

  fun lookUp (key: string) (env: env) =
    (* Types.URL (Types.HTTP {host=[], path=NONE, anchor=NONE}) *)
    case env of
       ref (_, Empty) => raise Control.NotFound
     | ref (keyList, Node((key', v), L, R)) => 
        if key = key' then v
        else if key > key' then lookUp key (ref (keyList,R):env)
        else lookUp key (ref (keyList,L):env)

  fun domain env =
    case !env of
      ([], _) => []
    | (list, _) => list

end

