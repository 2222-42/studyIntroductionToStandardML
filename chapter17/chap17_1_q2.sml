(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.1 Q17.2
*)

(*signature OS_FILE_SYS =
  sig
    type dirstream
    val openDir : string -> dirstream
    val readDir : dirstream -> string option
    val rewindDir : dirstream -> unit
    val closeDir : dirstream -> unit
    val chDir : string -> unit
    val getDir : unit -> string
    val mkDir : string -> unit
    val rmDir : string -> unit
    val isDir : string -> bool
    val isLink : string -> bool
    val readLink : string -> string
    val fullPath : string -> string
    val realPath : string -> string
    val fileSize : string -> Int64.int
    val modTime : string -> Time.time
    val setTime : string * Time.time option -> unit
    val remove : string -> unit
    val rename : {new:string, old:string} -> unit
    datatype access_mode = A_EXEC | A_READ | A_WRITE
    val access : string * access_mode list -> bool
    val tmpName : unit -> string
    eqtype file_id
    val fileId : string -> file_id
    val hash : file_id -> word
    val compare : file_id * file_id -> order
  end *)

(* signature OS_PATH =     
  sig
    exception Path      
    exception InvalidArc
    val parentArc : string
    val currentArc : string
    val fromString : string -> {arcs:string list, isAbs:bool, vol:string}
    val toString : {arcs:string list, isAbs:bool, vol:string} -> string
    val validVolume : {isAbs:bool, vol:string} -> bool
    val getVolume : string -> string
    val getParent : string -> string
    val splitDirFile : string -> {dir:string, file:string}
    val joinDirFile : {dir:string, file:string} -> string
    val dir : string -> string
    val file : string -> string
    val splitBaseExt : string -> {base:string, ext:string option}
    val joinBaseExt : {base:string, ext:string option} -> string
    val base : string -> string
    val ext : string -> string option
    val mkCanonical : string -> string
    val isCanonical : string -> bool
    val mkAbsolute : {path:string, relativeTo:string} -> string
    val mkRelative : {path:string, relativeTo:string} -> string
    val isAbsolute : string -> bool
    val isRelative : string -> bool
    val isRoot : string -> bool
    val concat : string * string -> string
    val fromUnixPath : string -> string
    val toUnixPath : string -> string
  end *)

structure F = OS.FileSys;
structure P = OS.Path;

local
  open TextIO
in
    fun copyStream ins outs = 
        if endOfStream ins then ()
        else case input1 ins of 
                    SOME c => (output1(outs,c);
                            copyStream ins outs)
                | NONE => copyStream ins outs
    fun copyFile inf outf = 
        let val ins = openIn inf
            val outs = openOut outf
        in (copyStream ins outs;
            closeIn ins; closeOut outs)
        end;
end

fun copy a b = 
    if not (F.isDir a) then
        (copyFile a b; F.setTime (b, SOME (F.modTime a)))
    else
        let val d = F.openDir a
            fun copyDirStream d b =
                let
                  val item = F.readDir d
                in
                  if item = NONE then F.closeDir d
                  else let
                    val from = P.concat(a,valOf(item))
                    val to = P.concat(b,valOf(item))
                  in
                    (copy from to; F.setTime (to, SOME (F.modTime from)); copyDirStream d b)
                  end
                end
        in 
            (F.mkDir b; copyDirStream d b)
        end;

(* 筆者の回答 *)
   fun copyByAuthor a b =
       if not (F.isDir a) then
         (copyFile a b;
          F.setTime (b,SOME (F.modTime a)))
       else
         let val d = F.openDir a
             fun copyDirStream d b =
                 case F.readDir d of
                   NONE => F.closeDir d
                 | SOME item =>
                   let val from = P.concat (a,item)
                       val to = P.concat (b,item)
                   in (copy from to;copyDirStream d b)
                   end
         in
           (F.mkDir b;
            copyDirStream d b;
            F.setTime (b,SOME (F.modTime a)))
         end;
val nowDirStr = F.getDir();
val newDir = F.openDir(nowDirStr^"/chapter17");
F.chDir (nowDirStr^"/chapter17");
(* copy "test.txt" "copied.txt";
copy "folder" "copiedFolder"; *)
