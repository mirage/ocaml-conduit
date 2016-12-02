open Mirage

let main = foreign ~deps:[abstract nocrypto] "Unikernel.Client" (console @-> job)

let () =
  register
    ~libraries:["conduit.mirage"; "vchan.xen"]
    ~packages:["conduit"; "vchan"]
    "vchan_client" [ main $ default_console ]
