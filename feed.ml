type entry = { title : string; link : string; date : int }

type doc_tree = E of string * (string * string) list * doc_tree list | D of string

exception Parse_error of string

let parse_doc_tree s =
  let input = Xmlm.make_input (`String(0, s)) in
  let el ((_, tag), a) children = E (tag, List.map (fun ((_, k), v) -> (k, v)) a, children)  in
  let data d = D d in
  snd (Xmlm.input_doc_tree ~el ~data input)

let rec parse_item = function
| [] -> { title = ""; link = ""; date = 0 }
| E ("title", _, [D title]) :: t -> { (parse_item t) with title }
| E ("link", _, [D link]) :: t -> { (parse_item t) with link }
| E ("pubDate", _, [D date]) :: t -> { (parse_item t) with date = Date.parse date }
| _ :: t -> parse_item t

let rec parse_channel = function
| [] -> []
| E ("item", _, i) :: t -> parse_item i :: parse_channel t
| _ :: t -> parse_channel t
  
let parse_rss = function
| [E ("channel", _, c)] | [D _; E ("channel", _, c); D _]
| [D _; E ("channel", _, c)] | [E ("channel", _, c); D _] -> parse_channel c
| _ -> raise (Parse_error "RSS feed must contain a single channel")

let rec parse_entry = function
| [] -> { title = ""; link = ""; date = 0 }
| E ("title", _, [D title]) :: t -> { (parse_entry t) with title }
| E ("link", a, []) :: t -> { (parse_entry t) with link = List.assoc "href" a }
| E ("updated", _, [D date]) :: t -> { (parse_entry t) with date = Date.parse date }
| _ :: t -> parse_entry t

let rec parse_atom = function
| [] -> []
| E ("entry", _, e) :: t -> parse_entry e :: parse_atom t
| _ ::t -> parse_atom t

let parse s = match parse_doc_tree s with
| E ("rss", _, r) -> parse_rss r
| E ("feed", _, a) -> parse_atom a
| _ -> raise (Parse_error "Feed is not RSS or Atom")

let merge feeds =
  List.sort (fun a b -> b.date - a.date) (List.concat feeds)
