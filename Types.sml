structure Types = struct 
  datatype expr = 
      STREXP of string 
    | IDEXP of string
    | LINKEXP of expr
    | FOLLOWEXP of expr * int
  datatype statement = 
      VAL of string * expr
    | EXPR of expr
    | CD of expr
    | COPY of expr * expr
    | PRINT of expr
    | USE of string
    | HELP
    | ENV
  datatype url = 
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option, root: url}
  datatype value =
      URL of url
    | PAGE of {url: url, links: url list}
end


