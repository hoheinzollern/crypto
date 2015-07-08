
let mask_32 = 0xffffffff

let mylsr x y = ((x land mask_32) lsr y)

let get_int data i =
  (int_of_char (String.get data i)) lor
    ((int_of_char (String.get data (i+1))) lsl 8) lor
      ((int_of_char (String.get data (i+2))) lsl 16) lor
	((int_of_char (String.get data (i+3))) lsl 24)

let murmur key seed =
  let len = String.length key in
  let m = 0x5bd1e995 in
  let r = 24 in
  let h = seed lxor len in
  let data = key in
  let rec murmur_loop h i =
    if i <= len-4 then
      begin
	let k = get_int data i in
	let k = k * m in
	let k = k lxor (mylsr k r) in
	let k = k * m in
	let h = h * m in
	let h = h lxor k in
	murmur_loop h (i+4)
      end
    else
      begin
	let rem = len mod 4 in
	let h = if rem >= 3 then h lxor ((int_of_char (Bytes.get data (i+2))) lsl 16) else h in
	let h = if rem >= 2 then h lxor ((int_of_char (Bytes.get data (i+1))) lsl 8) else h in
	let h = if rem >= 1 then h lxor (int_of_char (Bytes.get data (i))) else h in
	let h = h * m in h
      end in
  let h = murmur_loop h 0 in
  let h = h lxor (mylsr h 13) in
  let h = h * m in
  let h = h lxor (mylsr h 15) in
  h land mask_32

let bytes s =
  let h = murmur s 0 in
  let b = Bytes.create 4 in
  Bytes.set b 0 (char_of_int (h land 0xff));
  Bytes.set b 1 (char_of_int ((h lsr 8) land 0xff));
  Bytes.set b 2 (char_of_int ((h lsr 16) land 0xff));
  Bytes.set b 3 (char_of_int ((h lsr 24) land 0xff));
  b

let h = murmur "hello" 1;;
Printf.printf "And the hash is: %x\n" h
