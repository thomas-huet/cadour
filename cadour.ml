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

let split s = match String.index_opt s ' ' with
| None -> s, None
| Some i -> String.sub s 0 i, Some(String.sub s (i + 1) (String.length s - i - 1))

let feed_of_line (url, filter) =
  let xml = http_get url in
  Feed.filter filter (Feed.parse xml)

let log_error (url, _) e = Printf.eprintf "Error processing \"%s\": %s\n" url (Printexc.to_string e)

let rec map_err log f = function
| [] -> []
| h :: t ->
  try
    f h :: map_err log f t
  with e ->
    log h e;
    map_err log f t

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
| [f] -> [link_to_blog f; Element("hr", [], [])]
| f :: t -> link_to_blog f :: Data " | " :: summary t

let format_date d =
  let y, m, d = Ptime.to_date d in
  Printf.sprintf "%04d-%02d-%02d" y m d

let html_of_entry e =
  Element("div", [], [
    Element("p", [], [Data (format_date e.Feed.date)]);
    Element("a", ["href", e.Feed.link], [
      Element("h2", [], [Data e.Feed.title]);
      Element("p", [], [Data (domain e.Feed.link)]);
    ]);
    Element("hr", [], []);
  ])

let () =
  let lines = List.map split (read_lines ()) in
  let feeds = map_err log_error feed_of_line lines in
  let html_summary = summary feeds in
  let entries = Feed.merge (List.map (fun f -> f.Feed.entries) feeds) in
  let html_entries = List.map html_of_entry entries in
  let out = new Netchannels.output_channel stdout in
  write out html_summary;
  write out html_entries;
  out#close_out ()
