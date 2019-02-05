open Syndic

type entry = { title : string; link : string; date : Date.t }

type t = {name : string; url : string; description : string; entries : entry list}

type doc_tree = E of string * (string * string) list * doc_tree list | D of string

exception Parse_error of string

let parse_doc_tree s =
  let input = Xmlm.make_input (`String(0, s)) in
  let el ((_, tag), a) children = E (tag, List.map (fun ((_, k), v) -> (k, v)) a, children)  in
  let data d = D d in
  snd (Xmlm.input_doc_tree ~el ~data input)

module RSS = struct
  let rec parse_item = function
  | [] -> { title = ""; link = ""; date = Date.epoch }
  | E ("title", _, [D title]) :: t -> { (parse_item t) with title }
  | E ("link", _, [D link]) :: t -> { (parse_item t) with link }
  | E ("pubDate", _, [D date]) :: t -> { (parse_item t) with date = Date.of_rfc822 date }
  | _ :: t -> parse_item t

  let rec parse_channel = function
  | [] -> { name = ""; url = ""; description = ""; entries = [] }
  | E ("title", _, [D name]) :: t -> { (parse_channel t) with name }
  | E ("link", _, [D url]) :: t -> { (parse_channel t) with url }
  | E ("description", _, [D description]) :: t -> { (parse_channel t) with description }
  | E ("item", _, i) :: t ->
    let feed = parse_channel t in
    { feed with entries = parse_item i :: feed.entries }
  | _ :: t -> parse_channel t
    
  let parse = function
  | [E ("channel", _, c)] | [D _; E ("channel", _, c); D _]
  | [D _; E ("channel", _, c)] | [E ("channel", _, c); D _] -> parse_channel c
  | _ -> raise (Parse_error "RSS feed must contain a single channel")
end

module Atom = struct
  let is_alternate a = match List.assoc_opt "rel" a with
  | None | Some "alternate" -> true
  | Some _ -> false

  let rec parse_entry = function
  | [] -> { title = ""; link = ""; date = Date.epoch }
  | E ("title", _, [D title]) :: t -> { (parse_entry t) with title }
  | E ("link", a, []) :: t when is_alternate a ->
    { (parse_entry t) with link = List.assoc "href" a }
  | E ("published", _, [D date]) :: t -> { (parse_entry t) with date = Date.of_rfc3339 date }
  | E ("updated", _, [D date]) :: t ->
    let entry = parse_entry t in
    if entry.date = Date.epoch then { entry with date = Date.of_rfc3339 date }
    else entry
  | _ :: t -> parse_entry t

  let rec parse = function
  | [] -> { name = ""; url = ""; description = ""; entries = [] }
  | E ("title", _, [D name]) :: t -> { (parse t) with name }
  | E ("link", a, []) :: t when is_alternate a ->
    { (parse t) with url = List.assoc "href" a }
  | E ("subtitle", _, [D description]) :: t -> { (parse t) with description }
  | E ("entry", _, e) :: t ->
    let feed = parse t in
    { feed with entries = parse_entry e :: feed.entries }
  | _ ::t -> parse t
end

let parse s = match parse_doc_tree s with
| E ("rss", _, r) -> RSS.parse r
| E ("feed", _, a) -> Atom.parse a
| _ -> raise (Parse_error "Feed is not RSS or Atom")

let filter = function
| None -> fun feed -> feed
| Some r ->
  let positive = r.[0] <> '-' in
  let regex = Str.regexp (if positive then r else String.sub r 1 (String.length r - 1)) in
  let valid e = try
    let _ = Str.search_forward regex e.title 0 in
    positive
  with Not_found -> not positive
  in
  fun feed -> { feed with entries = List.filter valid feed.entries }
 
let merge feeds =
  List.sort (fun a b -> Date.compare b.date a.date) (List.concat feeds)
