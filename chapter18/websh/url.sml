signature URL = sig
  val parseUrl : Types.url -> string -> Types.value
  val urlToString : Types.url -> string
  val canonicalUrl : Types.url -> Types.url
  val baseUrl : Types.url -> Types.url
  val nodeUrl : Types.url -> string option
  val pathUrl : Types.url -> string list
  val joinUrlFile : Types.url -> string -> Types.url
  val joinUrlPath : Types.url -> string list -> Types.url
  val isRelative: Types.url -> bool
end

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
          let val (p, a) = SS.splitl (fn #"#" => false | _ => true) s
          in (map SS.string 
                (SS.fields (fn c => c = #"/") p),
              if SS.isEmpty a then 
                NONE
              else SOME (SS.string (SS.triml 1 a))
          )
          end
        fun concatinateOptions listOption list = 
          case listOption of
             NONE => SOME list
           | SOME v => SOME (v @ list)
      in 
        case path of
           ("" :: L) => 
            (case root of
               HTTP{path=existPath,host,...} => HTTP{host=host, path=concatinateOptions existPath L, anchor=anchor}
             | FILE {path=existPath,...} => FILE{path=existPath@L, anchor=anchor}
             | RELATIVE {path=existPath,root=originalRoot,...} => RELATIVE {path=existPath@L, anchor=anchor, root=originalRoot})
         | _ => RELATIVE {path=path, anchor=anchor, root=root}
      end
      (* let 
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor, root=root}
      end *)
    fun parseUrl root s = 
      let 
        val s = SS.full s 
        val (scheme, body) = SS.splitl(fn c => c <> #":") s
      in
        if SS.isEmpty body then
          parseRelative scheme root
        else
          case lower (SS.string scheme) of
            "http" => HTTP (parseHttp body)
          | "file" => FILE (parseFile body)
          | _ => parseRelative scheme root
      end
    fun isFileName string =
      let
        fun splitCharsDot chrList = 
          case chrList of
             [] => ([], [])
           | c :: chrList' => 
              if c = #"." then ([], chrList') 
              else let val (l, r) = splitCharsDot chrList'
                   in (c::l, r)
                   end
        val (l, r) = splitCharsDot(String.explode (string))
      in
        not (length r = 0)
      end
    (* TODO: HTTPの場合のpathからの取得や削除の修正 *)
    fun baseUrl url = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            (* case path of
               NONE =>  *)
                  if length host = 0 then HTTP {host=host, path=path, anchor=anchor}
                  else if isFileName (List.last host) then  HTTP {host=List.take(host, length(host) - 1),path=path, anchor=anchor}
                  else HTTP {host=host, path=path, anchor=anchor}
             (* | SOME v => 
                  if length v = 0 then HTTP {host=host, path=path, anchor=anchor}
                  else if isFileName (List.last v) then  HTTP {host=host,path=SOME(List.take(v, length(v) - 1)), anchor=anchor}
                  else HTTP {host=host, path=path, anchor=anchor} *)
         (* HTTP {host=host, path=path, anchor=anchor} *)
       | FILE {path=path, anchor=anchor} => 
            if length path = 0 then FILE {path=path, anchor=anchor}
            else if isFileName (List.last path) then  FILE {path=List.take(path, length(path) - 1), anchor=anchor}
            else FILE {path=path, anchor=anchor}
       (* FILE {path=path, anchor=anchor} *)
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            if length path = 0 then RELATIVE {path=path, anchor=anchor, root=root}
            else if isFileName (List.last path) then  RELATIVE {path=List.take(path, length(path) - 1), anchor=anchor, root=root}
            else  RELATIVE {path=path, anchor=anchor, root=root}
          (* RELATIVE {path=path, anchor=anchor, root=root} *)
    fun nodeUrl url = 
      case url of 
         HTTP {host=host, path=path, anchor=anchor} => 
            if length host = 0 then NONE
            else if isFileName (List.last host) then SOME(List.last host)
            else NONE
       | FILE {path=path, anchor=anchor} => 
            if length path = 0 then NONE
            else if isFileName (List.last path) then SOME(List.last path)
            else NONE
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            if length path = 0 then NONE
            else if isFileName (List.last path) then SOME(List.last path)
            else NONE
    fun pathUrl url = 
      case url of 
         HTTP {host=host, path=path, ...} => 
            if isSome path then host @ valOf(path)
            else host
       | FILE {path=path, ...} => 
            path
       | RELATIVE {path=path, root=root,...} => 
            pathUrl root @ path
    fun joinUrlFile url fileName = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            HTTP {host=host@[fileName], path=path, anchor=anchor}
       | FILE {path=path, anchor=anchor} => 
            FILE {path=path@[fileName], anchor=anchor}
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            RELATIVE {path=path@[fileName], anchor=anchor, root=root}
    fun addToOrRemoveFromStringList sourceList relativeList = 
      foldl (fn (x, R) => if x = ".." then List.take(R, length(R) - 1)
                          else if x = "." then R
                          else R @ [x]) sourceList relativeList
    fun addToOrRemoveFromListOption sourceListOption relativeList = 
      case sourceListOption of
         SOME v => SOME(addToOrRemoveFromStringList v relativeList)
       | NONE => SOME(addToOrRemoveFromStringList [] relativeList)
    fun joinUrlPath url stringList = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            HTTP {host=host, path=(addToOrRemoveFromListOption path stringList), anchor=anchor}
       | FILE {path=path, anchor=anchor} => 
            FILE {path=(addToOrRemoveFromStringList path stringList), anchor=anchor}
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            RELATIVE {path=(addToOrRemoveFromStringList path stringList), anchor=anchor, root=root}
    fun isRelative url = 
      case url of
         HTTP _ => false
       | FILE _ => false
       | RELATIVE _ => true
    fun urlToString url = 
      let 
        fun splice (nil,_) = ""
          | splice ([x],_) = x
          | splice ((h::t),s) = h ^ s ^ splice (t,s);
      in
        case url of 
          HTTP {host=host, path=path, anchor=anchor} => 
            "http://" ^
            (splice (host, "/")) ^
            (case path of
              NONE => ""
            | SOME li => "/"^splice(li, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s)
        | FILE {path=path, anchor=anchor} => 
            "file:///" ^
            (splice (path, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s)
        | RELATIVE {path=path, anchor=anchor, root=root} =>
            (splice (path, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s) ^
            " [on " ^
            (urlToString root) ^ "]"
      end
    fun canonicalUrl url = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            let val canonicArcString = OS.Path.fromString(
              OS.Path.mkCanonical(
                OS.Path.toString{arcs=host, isAbs=true, vol=""}
              )
            )
            in 
              (fn {arcs,...} => HTTP{host=arcs, path=path, anchor=anchor})canonicArcString
            end
       | FILE {path=path, anchor=anchor} => 
            let val canonicArcString = OS.Path.fromString(
              OS.Path.mkCanonical(
                OS.Path.toString{arcs=path, isAbs=true, vol=""}
              )
            )
            in 
              (fn {arcs,...} => FILE{path=arcs, anchor=anchor})canonicArcString
            end
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            let 
              val canonicRootUrl = canonicalUrl root
              val canonicPathString = OS.Path.fromString(
                OS.Path.mkCanonical(
                  OS.Path.toString{arcs=path, isAbs=true, vol=""}
                )
              )
            in
              case canonicRootUrl of
                 HTTP {host=hostRoot, path=pathRoot, anchor=anchorRoot} => 
                  let
                    val canonicPathString = OS.Path.fromString(
                      OS.Path.mkCanonical(
                        OS.Path.toString{arcs=(hostRoot @ path), isAbs=true, vol=""}
                      )
                    )
                  in
                    (fn {arcs, ...} =>HTTP {host=arcs, path=pathRoot, anchor=anchor}) canonicPathString
                  end
               | FILE {path=pathRoot, anchor=anchorRoot} => 
                  let
                    val canonicPathString = OS.Path.fromString(
                      OS.Path.mkCanonical(
                        OS.Path.toString{arcs=(pathRoot @ path), isAbs=true, vol=""}
                      )
                    )
                  in
                    (fn {arcs, ...} =>FILE {path=arcs, anchor=anchor}) canonicPathString
                  end
            end
       
  end
end

