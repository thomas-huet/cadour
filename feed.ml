open Syndic

type t = Atom.feed

let string_of_xml xml =
  let b = Buffer.create 1024 in
  let rec traverse = function
  | XML.Data(_, s) -> Buffer.add_string b s
  | XML.Node(_, _, children) -> List.iter traverse children
  in
  List.iter traverse xml;
  Buffer.contents b

let string_of_text_construct : Atom.text_construct -> string = function
| Atom.Text s | Atom.Html(_,s) -> s
| Atom.Xhtml(_, xml) -> string_of_xml xml

let parse uri content =
  try
    Atom.parse ~self:uri ~xmlbase:uri (Xmlm.make_input (`String(0, content)))
  with Atom.Error.Error _ ->
    Rss2.to_atom ~self:uri @@
      Rss2.parse ~xmlbase:uri (Xmlm.make_input (`String(0, content)))

let link f =
  let link = List.find (fun l -> l.Atom.rel = Atom.Alternate) f.Atom.links in
  Uri.to_string link.Atom.href

let title f =
  string_of_text_construct f.Atom.title

let subtitle f = match f.Atom.subtitle with
| None -> None
| Some t -> Some(string_of_text_construct t)

module Entry = struct
  type t = Atom.entry

  let date e =
    let date = match e.Atom.published with
      | None -> e.Atom.updated
      | Some d -> d
    in
    let y, m, d = Ptime.to_date date in
    Printf.sprintf "%04d-%02d-%02d" y m d

  let link (e : t) =
    let link = List.find (fun l -> l.Atom.rel = Atom.Alternate) e.Atom.links in
    Uri.to_string link.Atom.href

  let title (e : t) =
    string_of_text_construct e.Atom.title
end

let merge l =
  let all = Atom.aggregate l in
  all.Atom.entries
