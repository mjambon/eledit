type 'a t = 'a list

(* Return [Some ((), s)] if the stream is empty where [s] is itself,
   else [None] *)
let empty x =
  match x with
      [] -> Some ((), x)
    | _ -> None

(* Return [Some (a, s)] where [a] is the first element of the stream
   and [s] the remaining stream, or [None] if the stream is empty. *)
let next = function
    [] -> None
  | hd :: tl -> Some (hd, tl)

(* Return the stream of the characters of the string parameter. *)
let of_string s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l :=  s.[i] :: !l
  done;
  !l
