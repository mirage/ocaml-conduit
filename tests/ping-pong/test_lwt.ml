(* XXX(dinosaure): we serialize tests by ourselves! *)

let pp_process_status ppf = function
  | Unix.WEXITED n -> Format.fprintf ppf "(WEXITED %d)" n
  | Unix.WSIGNALED n -> Format.fprintf ppf "(WSIGNALED %d)" n
  | Unix.WSTOPPED n -> Format.fprintf ppf "(WSTOPPED %d)" n

let res = ref true

let exit_success = 0

let exit_failure = 1

let properly_exited = function Unix.WEXITED 0 -> true | _ -> false

let () =
  let pid =
    Unix.create_process_env "./with_lwt.exe"
      [| "./with_lwt.exe"; "client0"; "client1"; "client2" |]
      [||] Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  res := !res && properly_exited status ;
  Format.printf ">>> with_lwt.exe: %a.\n%!" pp_process_status status ;

  let pid =
    Unix.create_process_env "./with_lwt.exe"
      [|
        "./with_lwt.exe";
        "--with-ssl";
        "server.pem";
        "server.key";
        "client0";
        "client1";
        "client2";
      |]
      [||] Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  res := !res && properly_exited status ;
  Format.printf ">>> with_lwt.exe --with-ssl: %a.\n%!" pp_process_status status ;

  let pid =
    Unix.create_process_env "./with_lwt.exe"
      [|
        "./with_lwt.exe";
        "--with-tls";
        "server.pem";
        "server.key";
        "client0";
        "client1";
        "client2";
      |]
      [||] Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  res := !res && properly_exited status ;
  Format.printf ">>> with_lwt.exe --with-tls: %a.\n%!" pp_process_status status ;

  if !res then exit exit_success else exit exit_failure
