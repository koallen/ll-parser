(* File ll_parser.ml *)

(* Exercise:

 Implement some LL parser for the following grammar.

 E ::= E + E | E - E | ( E ) | Id

 Your parser shall produce an AST (see below).

 Assumptions:
   - Expressions may consist of white space which shall be ignored.
   - A simple lexer is provided.

 Hints:

  - Remove left recursion from the grammar.
  - Translate resulting grammar to a running program
    which checks if the input word is part of the grammar.
  - For generation of AST, apply the method discussed in class
    connected to attribute grammars (AGs).
 *)


(* Simple lexer *)
type token = PLUS | MINUS | OPEN | CLOSE | ID of char | EOF

let rec nextToken s =
     if String.length s == 0
     then (EOF, "")
     else   let rest = String.sub s 1 ((String.length s)-1) in
            let ch = String.get s 0 in
            match ch with
                   | ' '  -> nextToken rest
                   | '-'  -> (MINUS, rest)
                   | '+'  -> (PLUS, rest)
                   | '('  -> (OPEN, rest)
                   | ')'  -> (CLOSE, rest)
                   | x    -> (ID x, rest)


(* AST *)
type exp = Plus of exp*exp | Minus of exp*exp | Id of char

(* LL Praser *)
let rec e s =
    let token = nextToken s in
    match token with
        | (ID x, rest) -> a (Id x) rest
        | (OPEN, rest) -> let (t, r) = e rest in
                          (match nextToken r with
                              | (CLOSE, rest) -> (match t with
                                                     | None -> (None, rest)
                                                     | Some id -> a id rest)
                              | (_, rest) -> (None, rest))
        | (_, rest)    -> (None, rest)
and a id s =
    let token = nextToken s in
    match token with
        | (PLUS, rest)  -> let (t, r) = e rest in
                           (match t with
                               | None -> (None, r)
                               | Some t -> (Some (Plus (id, t)), r))
        | (MINUS, rest) -> let (t, r) = e rest in
                           (match t with
                               | None -> (None, r)
                               | Some t -> (Some (Minus (id, t)), r))
        | (EOF, "") -> (Some id, "")
        | (_, rest) -> (Some id, s)

(* A wrapper for the parser *)
let parse s =
    let (result, r) = e s in
    result
