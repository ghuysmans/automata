type t = Unix.sockaddr

let fd =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_BROADCAST true;
  fd

let create port =
  let ai = Unix.getaddrinfo "255.255.255.255" (string_of_int port) [] in
  (* FIXME? *)
  (List.hd ai).Unix.ai_addr

let send dest s =
  Unix.sendto fd s 0 (String.length s) [] dest |> ignore;
