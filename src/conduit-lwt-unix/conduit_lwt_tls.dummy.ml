module X509 = struct
  let private_of_pems ~cert:_ ~priv_key:_ = failwith "Tls not available"

  type authenticator = unit

  let default_authenticator = lazy ()
end

module Client = struct
  let connect ?src:_ ?certificates:_ ~authenticator:_ _host _sa =
    failwith "Tls not available"

  let tunnel ?certificates:_ ~authenticator:_ _host _ioc =
    failwith "Tls not available"
end

module Server = struct
  let init' ?backlog:_ ?stop:_ ?timeout:_ _tls _sa _callback =
    failwith "Tls not available"

  let init ?backlog:_ ~certfile:_ ~keyfile:_ ?stop:_ ?timeout:_ _sa _callback =
    failwith "Tls not available"
end

let available = false
