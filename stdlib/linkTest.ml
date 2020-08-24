type t

let leaf (n: int): t = Obj.obj (Obj.repr n)

external link: t -> t = "caml_testlink_link"

let next (l:t) = 
  let rt_l = Obj.repr l in
  assert (Obj.tag rt_l == Obj.forward_tag);
  (Obj.obj (Obj.repr (Obj.field rt_l 1)) : t)

let destruct (l: t)
    ~(lf: int -> 'a) 
    ~(lk: t -> 'a) =
  let rt_l = Obj.repr l in
  if Obj.tag rt_l = Obj.int_tag then(
    lf (Obj.obj rt_l : int) 
  )
  else 
  begin
    assert (Obj.tag rt_l = Obj.forward_tag);
    lk (Obj.obj (Obj.repr (Obj.field rt_l 1)) : t)
  end

let rec build_link n =
  if n == 0 then leaf 1
  else link (build_link (n - 1))

let wrap l = link l 
  

let print_link l =  
  let rec print_link_rec l = 
    destruct l
      ~lf: (fun n -> print_int n)
      ~lk: (fun _ -> print_string "-> "; print_link_rec (next l))
  in
  print_link_rec l; print_newline ()


