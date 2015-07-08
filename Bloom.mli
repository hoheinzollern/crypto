
type hash = bytes
type bloom = bytes

val add: (bytes -> hash) -> int -> bytes -> bloom -> bloom
val check: (bytes -> hash) -> int -> bytes -> bloom -> bool
