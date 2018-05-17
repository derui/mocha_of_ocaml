(* Type of assertion *)
open Mocha_of_ocaml

val (>:-) : string -> (unit -> assertion Lwt.t) -> (module Test)
