type t

val parse : Uri.t -> string -> t

val link : t -> string

val title : t -> string

val subtitle : t -> string option

module Entry : sig
  type t

  val date : t -> string

  val link : t -> string

  val title : t -> string
end

val merge : t list -> Entry.t list
