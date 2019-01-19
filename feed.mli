exception Parse_error of string

type entry = { title : string; link : string; date : Syndic.Date.t }

type t = {name : string; url : string; description : string; entries : entry list}

val parse : string -> t

val merge : entry list list -> entry list
