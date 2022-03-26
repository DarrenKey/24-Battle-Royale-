type 'a t = ('a, bool) Hashtbl.t

let create size : 'a t = Hashtbl.create size

let add hashset elem =
  Hashtbl.add hashset (List.sort compare elem) false

let mem hashset elem = Hashtbl.mem hashset elem
