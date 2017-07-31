module Client = struct
  let connect ?src host sa =
    Lwt.fail_with "Tls not available"
end

module Server = struct
  let init' ?backlog ?stop ?timeout tls sa callback =
    Lwt.fail_with "Tls not available"

  let init ?backlog ~certfile ~keyfile ?stop ?timeout sa callback =
    Lwt.fail_with "Tls not available"
end

let available = false
