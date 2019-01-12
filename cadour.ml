open Nethtml
open Nethttp_client.Convenience

let () = Nettls_gnutls.init ()

let rec read_lines () =
  try
    let line = read_line () in
    line :: read_lines ()
  with End_of_file -> []

let feed_of_url url = try
  let xml = http_get url in
  Feed.parse xml
with e ->
  Printf.eprintf "Error parsing \"%s\": %s" url (Printexc.to_string e);
  []

let html_of_entry e =
  Element("div", [], [
    Element("p", [], [Data (Date.to_string e.Feed.date)]);
    Element("a", ["href", e.Feed.link], [
      Element("h2", [], [Data e.Feed.title]);
      Element("p", [], [Data e.Feed.link]);
    ]);
    Element("hr", [], []);
  ])

let () =
  let lines = read_lines () in
  let feeds = List.map feed_of_url lines in
  let feed = Feed.merge feeds in
  let html = List.map html_of_entry feed in
  let out = new Netchannels.output_channel stdout in
  write out html;
  out#close_out ()
