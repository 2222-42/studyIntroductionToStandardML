signature FILEUTIL =
sig
  val exists : string -> string -> bool
  val touchDir : {isAbs:bool, vol:string, arcs: string list} -> unit
end

structure FileUtil : FILEUTIL =
struct
  (* Q18.30 *)
  fun exists fileName dirName =
      let 
        val d = OS.FileSys.openDir dirName
        fun copyDirStream d list =
            case OS.FileSys.readDir d of
              NONE => list before OS.FileSys.closeDir d
            | SOME item =>
              copyDirStream d (list @ [item])
      in
        List.exists (fn x => x = fileName) (copyDirStream d [])
      end
end

(* 
FileUtil.exists "test.txt" "/mnt/e/SMLProject/studyIntroductionToStandardML/";

*)