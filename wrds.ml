open Core

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let () =
  let lines = In_channel.read_lines "words-I-like.txt" in
  let lines =
    List.cartesian_product lines lines
    |> List.filter ~f:(fun (a, b) -> not (String.equal a b))
    |> Array.of_list |> knuth_shuffle |> Array.to_list
  in
  List.take lines 5 |> List.iter ~f:(fun (a, b) -> print_endline (a ^ b))
