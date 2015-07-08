
type hash = bytes
type bloom = bytes

let compute_bit hash b_len =
  ((int_of_char (Bytes.get hash 0)) +
     ((int_of_char (Bytes.get hash 1)) lsl 8) +
     ((int_of_char (Bytes.get hash 2)) lsl 16) + 
     ((int_of_char (Bytes.get hash 3)) lsl 24))
  mod (b_len * 8)

let set_bit bytes hash =
  let b_len = Bytes.length bytes in
  let bit = compute_bit hash b_len in
  let i = bit / 8 in
  let j = bit mod 8 in
  let byte = Bytes.get bytes i in
  let byte' = char_of_int ((int_of_char byte) lor (1 lsl j)) in
  Bytes.set bytes i byte'

let is_set_bit bytes hash =
  let b_len = Bytes.length bytes in
  let bit = compute_bit hash b_len in
  let i = bit / 8 in
  let j = bit mod 8 in
  let byte = Bytes.get bytes i in
  ((int_of_char byte) land (1 lsl j)) > 0

let add h k x b =
  (* let b' = Bytes.copy b in *)
  let b' = b in
  let rec add' x i =
    if i < k then
      let y = h x in
      set_bit b' y;
      add' y (i+1) in
  add' x 0;
  b'

let check h k x b =
  let rec check' x i =
    if i < k then
      begin
	let y = h x in
	if is_set_bit b y then
	  check' y (i+1)
	else false
      end
    else true in
  check' x 0

let add_md5 = add (Digest.bytes)
let check_md5 = check (Digest.bytes)

let rec add_md5_list k l b =
  match l with
  | [] -> b
  | (x::l') -> add_md5_list k l' (add_md5 k x b)

let rec check_md5_list k l b =
  match l with
  | [] -> ()
  | (x::l') -> if check_md5 k x b then Printf.printf "%s found\n" x;
	       check_md5_list k l' b

