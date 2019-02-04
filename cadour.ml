open Nethtml
open Nethttp_client.Convenience

let () = Nettls_gnutls.init ()

let rec read_lines () =
  try
    let line = read_line () in
    if String.length line = 0 || line.[0] = '#' then
      read_lines ()
    else
      line :: read_lines ()
  with End_of_file -> []

let feed_of_url url =
  let xml = http_get url in
  let uri = Uri.of_string url in
  Feed.parse uri xml

let print_err url = function
| Syndic.Atom.Error.Error _ as e ->
  Printf.eprintf "Syndic error processing \"%s\": %s\n" url (Syndic.Atom.Error.to_string e)
| e ->
  Printf.eprintf "Error processing \"%s\": %s\n" url (Printexc.to_string e)

let rec map_err print_err f = function
| [] -> []
| h :: t ->
  try
    f h :: map_err print_err f t
  with e ->
    print_err h e;
    map_err print_err f t

let domain_regexp = Str.regexp "https?://\\([^/]+\\)"
let domain url =
  if Str.string_match domain_regexp url 0 then
    Str.matched_group 1 url
  else
    url

let link_to_blog f =
  let url = Feed.link f in
  let subtitle = match Feed.subtitle f with
  | Some s -> s
  | None -> domain url
  in
  Element("a", ["href", url; "title", subtitle], [Data(Feed.title f)])

let rec summary = function
| [] -> [Element("hr", [], [])]
| [f] -> [link_to_blog f; Element("hr", [], [])]
| f :: t -> link_to_blog f :: Data " | " :: summary t

let html_of_entry e =
  Element("div", [], [
    Element("p", [], [Data(Feed.Entry.date e)]);
    Element("a", ["href", Feed.Entry.link e], [
      Element("h2", [], [Data(Feed.Entry.title e)]);
      Element("p", [], [Data(domain (Feed.Entry.link e))]);
    ]);
    Element("hr", [], []);
  ])

let () =
  let lines = read_lines () in
  let feeds = map_err print_err feed_of_url lines in
  let html_summary = summary feeds in
  let entries = Feed.merge feeds in
  let html_entries = List.map html_of_entry entries in
  let out = new Netchannels.output_channel stdout in
  write out html_summary;
  write out html_entries;
  out#close_out ()
