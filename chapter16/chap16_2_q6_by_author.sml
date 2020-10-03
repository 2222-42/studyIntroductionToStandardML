(* SML source file. Copyright (c) by 2222-42 2020.
* Q16.6
*)

signature PARSE_URL = sig
  exception urlFormat
  datatype url = 
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | FTP of {host : string list, path : string list}
    | RELATIVE of {path : string list, anchor : string option, root : url option ref}
  val parseUrl : string -> url
end

   structure Url:PARSE_URL =
   struct
   local
     structure SS = Substring
   in
     exception urlFormat
     datatype url
       = HTTP of {host : string list, path : string list option,
                  anchor : string option}
       | FILE of {path : string list, anchor : string option}
       | FTP of {host : string list, path : string list}
       | RELATIVE of {path : string list, anchor : string option, root : url option ref}
     fun parseHttp s =
         let val s = if SS.isPrefix "://" s then
                       SS.triml 3 s
                     else raise urlFormat
             fun neq c x = not (x = c)
             fun eq c x = c = x
             val (host,body) = SS.splitl (neq #"/") s
             val domain = map SS.string (SS.tokens (eq #".") host)
             val (path,anchor) =
                 if SS.isEmpty body then (NONE,NONE)
                 else
                   let val (p,a) = SS.splitl (neq #"#") body
                   in (SOME (map SS.string (SS.tokens (eq #"/") p)),
                       if SS.isEmpty a then NONE
                       else SOME (SS.string (SS.triml 1 a)))
                   end
         in {host=domain, path=path, anchor=anchor}
         end
     fun parseFtp s =
         let val s = if SS.isPrefix "://" s then SS.triml 3 s else raise urlFormat
             val (host,path) = SS.splitl (fn #"/" => false | _ => true) s
             val domains = map SS.string (SS.tokens (fn c => c = #".") host)
             val dirs = map SS.string (SS.tokens (fn c => c = #"/") host)
         in  {host=domains, path=dirs}
         end
     fun parseRelative s =
         let val (path,anchor) =
                 let val (p,a) = SS.splitl (fn #"#" => false | _ => true) s
                 in (map SS.string (SS.fields (fn c => c = #"/") p),
                  if SS.isEmpty a then NONE else SOME (SS.string (SS.triml 1 a)))
              end
         in  {path=path,anchor=anchor,root = ref NONE}
         end
     fun parseFile s =
         let val s = if SS.isPrefix ":/" s then SS.triml 2 s else raise urlFormat
          val (path,anchor) =
                 let val (p,a) = SS.splitl (fn #"#" => false | _ => true) s
                 in (map SS.string (SS.tokens (fn c => c = #"/") p),
                  if SS.isEmpty a then NONE else SOME (SS.string (SS.triml 1 a)))
              end
         in  {path=path,anchor=anchor}
         end
     fun parseUrl s =
         let val s = SS.full s
             val (scheme,body) = SS.splitl (fn c => not (c = #":")) s
         in
           if SS.isEmpty body then
          RELATIVE (parseRelative scheme)
           else case SS.string scheme of
               "http" => HTTP (parseHttp body)
                | "file" => FILE (parseFile body)
                | "ftp" => FTP (parseFtp body)
                | _  => raise urlFormat
         end
   end
   end