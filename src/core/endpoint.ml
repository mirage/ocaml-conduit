type t = Domain of [ `host ] Domain_name.t | IP of Ipaddr.t

let pp ppf = function
  | Domain domain_name -> Domain_name.pp ppf domain_name
  | IP ip -> Ipaddr.pp ppf ip

let error_msg str = Error (`Msg str)

let error_msgf fmt = Format.kasprintf error_msg fmt

let of_string str =
  let ( >>= ) = Result.bind in
  match Domain_name.of_string str >>= Domain_name.host with
  | Ok domain_name -> Ok (Domain domain_name)
  | Error _err0 ->
  match Ipaddr.of_string str with
  | Ok ip -> Ok (IP ip)
  | Error _err1 -> error_msgf "Invalid endpoint: %s" str

let v str =
  match of_string str with Ok v -> v | Error (`Msg err) -> invalid_arg err

let to_string = function
  | Domain domain_name -> Domain_name.to_string domain_name
  | IP ip -> Ipaddr.to_string ip

let domain domain_name = Domain domain_name

let ip ip = IP ip

let compare a b =
  let sup = 1 and inf = -1 in
  match (a, b) with
  | Domain a, Domain b -> Domain_name.compare a b
  | Domain _, IP _ -> sup
  | IP _, Domain _ -> inf
  | IP a, IP b -> Ipaddr.compare a b

let equal a b = compare a b = 0
