type 'a slot =
| Empty
| Elem of { mutable x : 'a }


type 'a t = {
  mutable data : 'a slot array;
  mutable sz : int;
}

let make n x = {data=Array.init n (fun _ -> Elem {x}); sz=0}

let[@inline] create () = {data = [||]; sz = 0}

let[@inline] clear s = s.sz <- 0

let[@inline] shrink t i =
  assert (i >= 0);
  assert (i<=t.sz);
  t.sz <- i

let[@inline] unsafe_get data i =
  match Array.unsafe_get data i with
  | Empty -> assert false
  | Elem {x} -> x

let[@inline] pop t =
  if t.sz = 0 then invalid_arg "vec.pop";
  let last = t.sz - 1 in
  let x = unsafe_get t.data last in
  t.sz <- last;
  (* Array.unsafe_set t.data last Empty; *)
  x

let[@inline] size t = t.sz

let[@inline] is_empty t = t.sz = 0

let[@inline] is_full t = Array.length t.data = t.sz

let[@inline] copy t : _ t =
  let copy_slot = function
    | Empty -> Empty
    | Elem {x} -> Elem {x} in
  let data = Array.map copy_slot t.data in
  {t with data}

let resize_ t size =
  let arr' = Array.make size Empty in
  Array.blit t.data 0 arr' 0 t.sz;
  t.data <- arr';
  ()

let ensure_cap_ self n =
  if n > Array.length self.data then (
    let new_size = max n (2 * Array.length self.data) in
    resize_ self new_size
  )

let ensure_size_with self f n =
  ensure_cap_ self n;
  if n > self.sz then (
    for i=self.sz to n-1 do
      self.data.(i) <- Elem {x = f()};
    done;
    self.sz <- n
  )

let ensure_size self x n = ensure_size_with self (fun () -> x) n

(* grow the array *)
let[@inline never] grow_to_double_size t : unit =
  if Array.length t.data = Sys.max_array_length then (
    failwith "vec: cannot resize";
  );
  let size =
    min Sys.max_array_length (max 4 (2 * Array.length t.data))
  in
  let arr' = Array.make size Empty in
  Array.blit t.data 0 arr' 0 t.sz;
  t.data <- arr';
  assert (Array.length t.data > t.sz);
  ()

let[@inline] push t x : unit =
  if is_full t then grow_to_double_size t;
  Array.unsafe_set t.data t.sz (Elem {x});
  t.sz <- t.sz + 1

let[@inline] get t i =
  match Array.get t.data i with
  | Empty -> invalid_arg "vec.get"
  | Elem {x} -> x

let[@inline] set t i v =
  match Array.get t.data i with
  | Empty ->
    if i = t.sz then push t v
    else invalid_arg "vec.set"
  | Elem s -> s.x <- v

let[@inline] fast_remove t i =
  match Array.get t.data i with
  | Empty -> invalid_arg "fast_remove"
  | Elem s ->
    s.x <- unsafe_get t.data (t.sz - 1);
    (* Array.unsafe_set t.data (t.sz - 1) Empty; *)
    t.sz <- t.sz - 1

let filter_in_place f vec =
  let i = ref 0 in
  while !i < size vec do
    if f (unsafe_get vec.data !i) then incr i else fast_remove vec !i
  done

let sort t f : unit =
  let sub_arr = if is_full t then t.data else Array.sub t.data 0 t.sz in
  Array.fast_sort (fun s1 s2 ->
      match s1, s2 with
      | Empty, Empty -> 0
      | Empty, Elem _ -> -1
      | Elem _, Empty -> 1
      | Elem {x = x1}, Elem {x = x2} -> f x1 x2)
    sub_arr;
  t.data <- sub_arr

let[@inline] iter f t =
  for i = 0 to size t - 1 do
    f (unsafe_get t.data i)
  done

let append a b = iter (push a) b

let[@inline] iteri f t =
  for i = 0 to size t - 1 do
    f i (unsafe_get t.data i)
  done

let[@inline] to_iter a k = iter k a

let exists p t = Iter.exists p @@ to_iter t
let for_all p t = Iter.for_all p @@ to_iter t
let fold f acc a = Iter.fold f acc @@ to_iter a
let to_list a = Iter.to_list @@ to_iter a
let to_array a = Array.init a.sz (fun i -> unsafe_get a.data i)

let of_list l : _ t =
  match l with
  | [] -> create()
  | x :: tl ->
    let v = make (List.length tl+1) x in
    List.iter (push v) l;
    v

let pp ?(sep=", ") pp out v =
  let first = ref true in
  iter
    (fun x ->
       if !first then first := false else Format.fprintf out "%s@," sep;
       pp out x)
    v
