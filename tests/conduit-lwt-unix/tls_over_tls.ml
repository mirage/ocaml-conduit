open Lwt.Infix

let hostname = "mirage.io"

(* To test TLS-over-TLS, the `squid` proxy can be installed locally and configured to support HTTPS:

   - Generate a certificate for localhost: https://gist.github.com/cecilemuller/9492b848eb8fe46d462abeb26656c4f8

   $ openssl req -x509 -nodes -new -sha256 -days 1024 -newkey rsa:2048 -keyout RootCA.key -out RootCA.pem -subj "/C=US/CN=Example-Root-CA"
   $ openssl x509 -outform pem -in RootCA.pem -out RootCA.crt
   $ cat > domains.ext
   authorityKeyIdentifier=keyid,issuer
   basicConstraints=CA:FALSE
   keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
   subjectAltName = @alt_names
   [alt_names]
   DNS.1 = localhost
   $ openssl req -new -nodes -newkey rsa:2048 -keyout localhost.key -out localhost.csr -subj "/C=US/ST=YourState/L=YourCity/O=Example-Certificates/CN=localhost.local"
   $ openssl x509 -req -sha256 -days 1024 -in localhost.csr -CA RootCA.pem -CAkey RootCA.key -CAcreateserial -extfile domains.ext -out localhost.crt

   - Configure squid by adding HTTPS support on port 3129 in /etc/squid/squid.conf :

   https_port 3129 tls-cert=/path/to/localhost.crt tls-key=/path/to/localhost.key
*)

let proxy =
  `TLS
    (`Hostname "localhost", `IP (Ipaddr.of_string_exn "127.0.0.1"), `Port 3129)

let string_prefix ~prefix msg =
  let len = String.length prefix in
  String.length msg >= len && String.sub msg 0 len = prefix

let main () =
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.connect ~ctx proxy >>= fun (_flow, ic, oc) ->
  let req =
    String.concat "\r\n"
      [ "CONNECT " ^ hostname ^ ":443 HTTP/1.1"; "Host: " ^ hostname; ""; "" ]
  in
  Lwt_io.write oc req >>= fun () ->
  let rec try_read () =
    Lwt_io.read ic ~count:1024 >>= fun msg ->
    if msg = "" then try_read () else Lwt.return msg
  in
  try_read () >>= fun msg ->
  assert (string_prefix ~prefix:"HTTP/1.1 200 " msg);

  (* We are now connected to mirage.io:443 through the proxy *)
  let client = `TLS_tunnel (`Hostname hostname, ic, oc) in
  Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, ic, oc) ->
  let req =
    String.concat "\r\n" [ "GET / HTTP/1.1"; "Host: " ^ hostname; ""; "" ]
  in
  Lwt_io.write oc req >>= fun () ->
  Lwt_io.read ic ~count:4096 >>= fun msg ->
  Lwt_io.print msg >>= fun () ->
  Lwt_io.read ic ~count:4096 >>= fun msg ->
  Lwt_io.print msg >>= fun () -> Lwt_io.print "\n"

let () = Lwt_main.run (main ())
