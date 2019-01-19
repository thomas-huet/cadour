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

let feed_of_url url = try
  let xml = http_get url in
  Some(Feed.parse xml)
with e ->
  Printf.eprintf "Error parsing \"%s\": %s\n" url (Printexc.to_string e);
  None

let domain_regexp = Str.regexp "https?://\\([^/]+\\)"
let domain url =
  if Str.string_match domain_regexp url 0 then
    Str.matched_group 1 url
  else
    url

let title f =
  if f.Feed.description = "" then domain f.Feed.url
  else f.Feed.description

let link_to_blog f =
  Element("a", ["href", f.Feed.url; "title", title f], [Data f.Feed.name])

let rec summary = function
| [] -> [Element("hr", [], [])]
| None :: t -> summary t
| Some f :: None :: t -> summary (Some f :: t)
| [Some f] -> [link_to_blog f; Element("hr", [], [])]
| Some f :: t -> link_to_blog f :: Data " | " :: summary t

let html_of_entry e =
  Element("div", [], [
    Element("p", [], [Data (Date.to_string e.Feed.date)]);
    Element("a", ["href", e.Feed.link], [
      Element("h2", [], [Data e.Feed.title]);
      Element("p", [], [Data (domain e.Feed.link)]);
    ]);
    Element("hr", [], []);
  ])

let () =
  let lines = read_lines () in
  let feeds = List.map feed_of_url lines in
  let html_summary = summary feeds in
  let entries = Feed.merge (List.map (function None -> [] | Some f -> f.Feed.entries) feeds) in
  let html_entries = List.map html_of_entry entries in
  let out = new Netchannels.output_channel stdout in
  write out html_summary;
  write out html_entries;
  out#close_out ()
