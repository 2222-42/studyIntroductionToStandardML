signature FILEUTIL =
sig
  val exists : string -> string -> bool
  val touchDir : {isAbs:bool, vol:string, arcs: string list} -> unit
end