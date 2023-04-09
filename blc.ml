(* Untyped lambda calculus *)

(* Define a type for lambda terms *)
type term =
  | Var of int
  | Abs of term Lazy.t
  | App of term Lazy.t * term Lazy.t

(* Helper functions to construct lambda terms *)
let abs t = Abs (lazy t)
let app t1 t2 = App (lazy t1, lazy t2)

(* Church encoding of booleans *)
let t_true = abs (abs (Var 1))
let t_false = abs (abs (Var 0))

(* Shift all free variables by d, starting from c *)
let rec shift d c t = match t with
  | Var k -> Var (if k < c then k else k + d)
  | Abs t' -> Abs (lazy (shift d (c + 1) (Lazy.force t')))
  | App (t1, t2) -> App (lazy (shift d c (Lazy.force t1)), lazy (shift d c (Lazy.force t2)))

(* Substitute s for j in t *)
let rec subst j s t = match t with
  | Var k -> if k = j then s else Var k
  | Abs t' -> Abs (lazy (subst (j + 1) (shift 1 0 s) (Lazy.force t')))
  | App (t1, t2) -> App (lazy (subst j s (Lazy.force t1)), lazy (subst j s (Lazy.force t2)))

(* Check if a term is a Church encoding of a boolean *)
let is_church_bool t = match t with
  | Abs (lazy (Abs (lazy (Var 1)))) -> true (* Church encoding of true *)
  | Abs (lazy (Abs (lazy (Var 0)))) -> true (* Church encoding of false *)
  | _ -> false

(* Check if a term is a Church encoding of a pair of booleans *)
let is_church_bool_pair t = match t with
  | Abs (lazy (App (lazy (Var 0), lazy (App (m, n))))) when is_church_bool (Lazy.force m) -> true
  | _ -> false

(* Reduce a term to normal form. Use a call-by-need evaluation strategy, I think? *)
let rec reduce t =
    if is_church_bool_pair t then
        t
    else
        match t with
        | Var _ -> t
        (* Do we do that in call by need? Does that break it? *)
        | Abs t1 -> Abs (lazy (reduce (Lazy.force t1)))
        | App (t1, t2) ->
            let t1_eval = reduce (Lazy.force t1) in
            match t1_eval with
            | Abs t' -> reduce (subst 0 (shift 1 0 (Lazy.force t2)) (Lazy.force t'))
            | _ -> App (lazy t1_eval, t2)

(* Pretty print a lambda term *)
let rec pretty_print (t : term) (indent : string) (prefix : string) : unit =
    let next_indent = match prefix with
        | "├─" -> indent ^ "│   "
        | "└─" -> indent ^ "    "
        | _ -> indent ^ "    " in
    match t with
    | Var v ->
        Printf.printf "%s%s Var %d\n" indent prefix v
    | Abs (lazy t1) ->
        Printf.printf "%s%s Abs\n" indent prefix;
        pretty_print t1 next_indent "└─"
    | App (lazy t1, lazy t2) ->
        Printf.printf "%s%s App\n" indent prefix;
        pretty_print t1 next_indent "├─";
        pretty_print t2 next_indent "└─"

(* Read BLC string and convert it to a lambda term *)
let rec term_of_blc blc =
    let rec var_of_blc idx blc =
      match blc with
      | '0'::rest -> (Var idx, rest)
      | '1'::rest -> var_of_blc (idx + 1) rest
      | _ -> failwith "Invalid BLC input" in
    match blc with
    | '0'::'1'::rest ->
        let (t1, rest1) = term_of_blc rest in
        let (t2, rest2) = term_of_blc rest1 in
        (App (lazy t1, lazy t2), rest2)
    | '0'::'0'::rest ->
        let (t, rest1) = term_of_blc rest in
        (Abs (lazy t), rest1)
    | '1'::rest ->
        var_of_blc 0 rest
    | _ -> failwith "Invalid BLC input"

  (* Inefficiently convert a lambda term to a BLC string *)
  let rec blc_of_term term =
    match term with
    | Var v -> "1" ^ (String.make (v+1) '0') ^ "1"
    | Abs (lazy t) -> "01" ^ blc_of_term t
    | App (lazy t1, lazy t2) -> "00" ^ blc_of_term t1 ^ blc_of_term t2

(* Helper functions to call term_of_blc and blc_of_term *)
let read_blc s = fst (term_of_blc (s |> String.to_seq |> List.of_seq))
let write_blc term = blc_of_term term

(* BLC booleans *)
let blc_true = "0000110"        (* λt.λf.t *)
let blc_false = "000010"      (* λt.λf.f *)


(* BLC pairs *)
(*
let blc_pair = "010101001"       (* λa.λb.λf.fab *)
let blc_first = "0100101001"     (* λp.p(λa.λb.a) *)
let blc_second = "01001010001"    (* λp.p(λa.λb.b) *) *)

(* Read BLC constructors into lambda terms *)
let term_true = read_blc blc_true
let term_false = read_blc blc_false

let omega = app (abs (app (Var 0) (Var 0))) (abs (app (Var 0) (Var 0)))

let primes_blc = "00010001100110010100011010000000010110000010010001010111110111101001000110100001110011010000000000101101110011100111111101111000000001111100110111000000101100000110110"

let primes_term = read_blc primes_blc
let r = reduce primes_term


let _ =

    pretty_print r "" "";
    Printf.printf "%b\n" (is_church_bool_pair r)
