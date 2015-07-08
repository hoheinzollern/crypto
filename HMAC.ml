open Printf
let bytexor x y = char_of_int ((int_of_char x) lxor (int_of_char y))

let bxor x y =
  let len_x = Bytes.length x in
  let len_y = Bytes.length y in
  let len = max len_x len_y in
  let z = Bytes.create len in
  let rec xor i =
    if i < len_x && i < len_y then
      begin
	let v = bytexor (Bytes.get x i) (Bytes.get y i) in
	Bytes.set z i v;
	xor (i+1)
      end
    else if i < len_x then
      begin 
	Bytes.set z i (Bytes.get x i);
	xor (i+1)
      end
    else if i < len_y then
      begin 
	Bytes.set z i (Bytes.get y i);
	xor (i+1)
      end
    else () in
  xor 0; z;;

let hex_digits_low = "0123456789abcdef"
let hex_digits_up  = "0123456789ABCDEF"

let repr_hex s =
  let len_s = String.length s in
  let repr_s = Bytes.create (len_s * 4) in
  Bytes.iteri (fun i c ->
	       let c = int_of_char c in
	       let c1 = String.get hex_digits_low (c lsr 0x4) in
	       let c2 = String.get hex_digits_low (c land 0xf) in
	       Bytes.set repr_s (i * 2) c1;
	       Bytes.set repr_s (i * 2 + 1) c2) s;
  repr_s;;

let print_hex s =
  print_string (repr_hex s);;

let cx36 = '\x36';;
let cx5c = '\x5c';;

let build_pad pad (fill: char) =
  let n = Bytes.length pad in
  let rec build_pad i =
    if i < n then
      begin
	Bytes.set pad i fill;
	build_pad (i+1)
      end
    else ()
  in build_pad 0;;

let hmac h =
  let len_h = String.length (h "") in
  let ipad = Bytes.create len_h in
  build_pad ipad cx36;
  let opad = Bytes.create len_h in
  build_pad opad cx5c;
  fun k ->
  let len_k = String.length k in
  let k' = if len_k = len_h then k
	   else if len_k < len_h then
	     begin
	       let ext = len_h - len_k in
	       let k' = Bytes.extend k 0 ext in
	       Bytes.fill k' len_k ext (char_of_int 0); k'
	     end
	   else (* len_k > len_h *)
	     h k
  in
  print_string (repr_hex k' ^ "\n");
  let i_k_pad = bxor k' ipad in
  let o_k_pad = bxor k' opad in
  fun m ->
  h (Bytes.cat o_k_pad (h (Bytes.cat i_k_pad m)));;

let md5 = Digest.string;;

let hmac_md5 = hmac md5;;

let sha1 x = Sha1.to_bin (Sha1.string x)
let sha256 x = Sha256.to_bin (Sha256.string x)

let hmac_sha1 = hmac sha1;;
let hmac_sha256 = hmac sha256;;

print_hex (hmac_md5 "key" "pass");
print_string "\n";
print_hex (hmac_sha1 "key" "pass");
print_string "\n";
print_hex (hmac_sha256 "key" "pass");
print_string "\n";
