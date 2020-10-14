use "./Types.sml";
use "./Control.sml";
fun isUpper c = #"A" <= c andalso c <= #"Z"

fun toLower c =
    if isUpper c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;
fun lower s = implode (map toLower (explode s))

structure Url = struct
  local
    open Types Control
  in
    structure SS = Substring
    exception urlFormat
    fun neq c x = not (x = c)
    fun eq c x = c = x
    fun parseHttp s = 
      let
        val s = if SS.isPrefix "://"  s then
                  SS.triml 3 s
                else raise urlFormat
        val (host, body) = SS.splitl (neq #"/") s
        val domain = map SS.string (SS.tokens (eq #".") host)
        val (path, anchor) =
          if SS.isEmpty body then (NONE, NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") body
            in (SOME (map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {host=domain, path=path, anchor=anchor}
      end
    fun parseFile s = 
      let 
        val s = if SS.isPrefix ":/"  s then
            SS.triml 2 s
          else raise urlFormat
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor}
      end
    fun parseRelative s root = 
      let 
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor, root=root}
      end
    fun parseUrl (s, root) = 
      let 
        val s = SS.full s 
        val (scheme, body) = SS.splitl(fn c => c <> #":") s
      in
        if SS.isEmpty body then
          RELATIVE (parseRelative scheme (FILE{path=[], anchor=NONE}))
        else
          case lower (SS.string scheme) of
            "http" => HTTP (parseHttp body)
          | "file" => FILE (parseFile body)
          | _ => raise urlFormat
      end
  end
end
