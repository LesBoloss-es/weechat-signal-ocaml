type t = {socket: Unix.file_descr; ic: in_channel}


let create path =
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.connect socket (Unix.ADDR_UNIX path);
  let ic = Unix.in_channel_of_descr socket in
  {socket; ic}


let close sock = close_in sock.ic


let print_line sock =
  Format.kasprintf
    (fun s ->
      try
        let _ = Unix.write_substring sock.socket s 0 (String.length s) in
        let _ = Unix.write_substring sock.socket "\n" 0 1 in
        Ok ()
      with Unix.Unix_error _ as e ->
        Error (Printexc.to_string e))


let subscribe sock username =
  print_line sock {|{"type": "subscribe", "username": "%s"}|} username

let list_groups sock username =
  print_line sock {|{"type": "list_groups", "username": "%s"}|} username

let list_contacts sock username =
  print_line sock {|{"type": "list_contacts", "username": "%s"}|} username
