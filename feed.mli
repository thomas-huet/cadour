exception Parse_error of string

type entry = { title : string; link : string; date : int }

val parse : string -> entry list

val merge : entry list list -> entry list
