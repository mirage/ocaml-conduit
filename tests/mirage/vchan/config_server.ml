open Mirage

let main = foreign ~deps:[abstract nocrypto] "Unikernel.Server" (console @-> job)

let () =
  register
    ~libraries:["conduit.mirage"; "vchan.xen"]
    ~packages:["conduit"; "vchan"]
    "vchan_server" [ main $ default_console ]
