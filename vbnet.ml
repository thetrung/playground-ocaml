
(* ==============================================================================
   1. ABSTRACT SYNTAX TREE (AST) DEFINITIONS
   ============================================================================== *)
type data_type = Integer | Single | Double | Byte | Long

type expr =
  | Literal of string
  | Variable of string
  | BinOp of expr * string * expr

type stmt =
  | VarDecl of string * data_type * expr option
  | Assign of string * expr
  | Return of expr

type field_decl = string * data_type * int option

type program_element =
  | Structure of string * field_decl list
  | Function of string * (string * data_type) list * data_type * stmt list

type program = program_element list

let rec print_type = function

  | Integer -> "Integer" | Single -> "Single" | Double -> "Double" | Byte -> "Byte" | Long -> "Long"

(* ==============================================================================
   2. TOKENS DEFINITIONS
   ============================================================================== *)
type token =
  | KEYWORD of string
  | TYPE of data_type
  | ID of string
  | NUMBER of string
  | ASSIGN
  | OP of string
  | LPAREN
  | RPAREN
  | NEWLINE
  | EOF

(* ==============================================================================
   3. THE REAL CHARACTER-BY-CHARACTER LEXER (From String)
   ============================================================================== *)
exception LexError of string

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_space c = c = ' ' || c = '\t' || c = '\r'

let lex code =
  let len = String.length code in
  let pos = ref 0 in
  let tokens = ref [] in

  let peek_char () = if !pos < len then Some code.[!pos] else None in
  let advance () = incr pos in

  while !pos < len do
    match peek_char() with
    | None -> ()
    | Some c when is_space c -> advance ()
    
    (* Handle Comments: In VB, a single quote (') skips the whole line *)
    | Some '\'' ->
        while !pos < len && code.[!pos] <> '\n' do advance () done
        
    (* Handle Newlines explicitly *)
    | Some '\n' ->
        tokens := NEWLINE :: !tokens;
        advance ()
        
    (* Symbols & Operators *)
    | Some '=' -> tokens := ASSIGN :: !tokens; advance ()
    | Some '(' -> tokens := LPAREN :: !tokens; advance ()
    | Some ')' -> tokens := RPAREN :: !tokens; advance ()

    | Some ('+' | '-' | '*' | '/' | '%' as op) -> 
        tokens := OP (String.make 1 op) :: !tokens; 
        advance ()
        
    (* Numbers *)
    | Some c when is_digit c ->
        let start = !pos in
        while !pos < len && is_digit code.[!pos] do advance () done;
        let num = String.sub code start (!pos - start) in
        tokens := NUMBER num :: !tokens
        
    (* Identifiers, Keywords, and Data Types *)
    | Some c when is_alpha c ->
        let start = !pos in
        while !pos < len && (is_alpha code.[!pos] || is_digit code.[!pos]) do advance () done;
        let word = String.sub code start (!pos - start) |> String.lowercase_ascii in
        
        let tok = match word with

          | "structure" | "end" | "function" | "as" | "dim" | "public" | "if" | "then" | "else" | "return" -> KEYWORD word
          | "integer" -> TYPE Integer
          | "single"  -> TYPE Single
          | "double"  -> TYPE Double
          | "byte"    -> TYPE Byte
          | "long"    -> TYPE Long
          | _ -> ID word
        in
        tokens := tok :: !tokens
        
    | Some unknown -> 
        raise (LexError (Printf.sprintf "Unknown character encountered: '%c'" unknown))
  done;
  List.rev (EOF :: !tokens)

(* ==============================================================================
   4. THE PARSER ENGINE
   ============================================================================== *)
exception ParseError of string

(* Converts a single token variant into a readable string string layout *)
let string_of_token = function
  | KEYWORD kw -> Printf.sprintf "KEYWORD(%s)" kw
  | TYPE dt    -> Printf.sprintf "TYPE(%s)" (print_type dt) (* Uses your existing print_type *)
  | ID name    -> Printf.sprintf "ID(%s)" name
  | NUMBER n   -> Printf.sprintf "NUMBER(%s)" n
  | ASSIGN     -> "ASSIGN(=)"
  | OP op      -> Printf.sprintf "OP(%s)" op
  | LPAREN     -> "LPAREN(()"
  | RPAREN     -> "RPAREN())"
  | NEWLINE    -> "NEWLINE(\\n)"
  | EOF        -> "EOF"

(* Combines a complete list of tokens into a single clean text block *)
let string_of_token_list tokens =
  tokens 
  |> List.map string_of_token 
  |> String.concat ", "

type parser_state = { mutable tokens : token list }

let peek state = match state.tokens with [] -> EOF | t :: _ -> t

let consume state expected =
  match state.tokens with
  | t :: ts when t = expected -> state.tokens <- ts
  | e -> raise (ParseError ("Unexpected token in: \n" ^ (string_of_token_list e) ^ " mismatch error"))

let consume_id state =
  match state.tokens with ID name :: ts -> state.tokens <- ts; name | _ -> raise (ParseError "Expected an Identifier")

let consume_type state =
  match state.tokens with TYPE dt :: ts -> state.tokens <- ts; dt | _ -> raise (ParseError "Expected a valid Core Type")

let consume_number state =
  match state.tokens with NUMBER n :: ts -> state.tokens <- ts; n | _ -> raise (ParseError "Expected base digits")

let rec skip_newlines state =
  match peek state with NEWLINE -> state.tokens <- List.tl state.tokens; skip_newlines state | _ -> ()

let parse_structure state =
  consume state (KEYWORD "structure");
  let struct_name = consume_id state in
  consume state NEWLINE;
  
  let rec parse_fields acc =
    skip_newlines state;
    match peek state with
    | KEYWORD "end" ->
        consume state (KEYWORD "end");
        consume state (KEYWORD "structure");
        consume state NEWLINE;
        List.rev acc
    | _ ->
        if peek state = KEYWORD "public" then consume state (KEYWORD "public");
        let field_name = consume_id state in
        let array_size = 
          if peek state = LPAREN then begin
            consume state LPAREN;
            let size = int_of_string (consume_number state) in
            consume state RPAREN;
            Some (size + 1)
          end else None 
        in
        consume state (KEYWORD "as");
        let field_type = consume_type state in
        consume state NEWLINE;
        parse_fields ((field_name, field_type, array_size) :: acc)
  in
  Structure (struct_name, parse_fields [])

let parse_primary state =
  match peek state with
  | NUMBER n -> state.tokens <- List.tl state.tokens; Literal n
  | ID name -> state.tokens <- List.tl state.tokens; Variable name
  | _ -> raise (ParseError "Invalid syntax expression target")

let rec parse_expr state =
  let left = parse_primary state in
  match peek state with
  | OP op ->
      state.tokens <- List.tl state.tokens;
      let right = parse_primary state in
      BinOp (left, op, right)
  | _ -> left

let parse_statement state =
  skip_newlines state;
  match peek state with
  | KEYWORD "dim" ->
      consume state (KEYWORD "dim");
      let name = consume_id state in
      consume state (KEYWORD "as");
      let dt = consume_type state in
      let init_expr = if peek state = ASSIGN then (consume state ASSIGN; Some (parse_expr state)) else None in
      consume state NEWLINE;
      VarDecl (name, dt, init_expr)
  | KEYWORD "return" ->
      consume state (KEYWORD "return");
      let expr = parse_expr state in
      consume state NEWLINE;
      Return expr
  | ID target ->
      state.tokens <- List.tl state.tokens;
      consume state ASSIGN;
      let expr = parse_expr state in
      consume state NEWLINE;
      Assign (target, expr)
  | _ -> raise (ParseError "Invalid internal execution statement")

let parse_function state =
  consume state (KEYWORD "function");
  let func_name = consume_id state in
  consume state LPAREN;
  let rec parse_params acc =
    match peek state with
    | RPAREN -> consume state RPAREN; List.rev acc
    | ID name ->
        state.tokens <- List.tl state.tokens;
        consume state (KEYWORD "as");
        let dt = consume_type state in
        parse_params ((name, dt) :: acc)
    | _ -> raise (ParseError "Bad function parameter layout")
  in
  let params = parse_params [] in
  consume state (KEYWORD "as");
  let return_type = consume_type state in
  consume state NEWLINE;
  
  let rec parse_body acc =
    skip_newlines state;
    match peek state with
    | KEYWORD "end" ->
        consume state (KEYWORD "end");
        consume state (KEYWORD "function");
        consume state NEWLINE;
        List.rev acc
    | _ ->
        let stmt = parse_statement state in
        parse_body (stmt :: acc)
  in
  Function (func_name, params, return_type, parse_body [])

let parse_program state =
  let rec parse_elements acc =
    skip_newlines state;
    match peek state with
    | EOF -> List.rev acc
    | KEYWORD "public" -> consume state (KEYWORD "public"); parse_elements acc
    | KEYWORD "structure" -> parse_elements (parse_structure state :: acc)
    | KEYWORD "function" -> parse_elements (parse_function state :: acc)
    | _ -> raise (ParseError "Global context allows structures and functions only")
  in
  parse_elements []

(* ==============================================================================
   5. VISUAL PRINT TREE FORMATTER
   ============================================================================== *)
let print_ast program =
  List.iter (fun element ->
    match element with
    | Structure (name, fields) ->
        Printf.printf "-> Native Struct Found: '%s'\n" name;
        List.iter (fun (f_name, f_type, array_size) ->
          match array_size with
          | Some size -> Printf.printf "   * Buffer field: %s As %s[%d bytes]\n" f_name (print_type f_type) size
          | None      -> Printf.printf "   * Standard field: %s As %s\n" f_name (print_type f_type)
        ) fields
    | Function (name, params, ret_type, _) ->
        Printf.printf "-> Native Function Found: '%s' returning %s\n" name (print_type ret_type);
        Printf.printf "   * Dynamic inline parameters count: %d\n" (List.length params)
  ) program

(* ==============================================================================
   6. REAL COMPILER TEST EXECUTION (Parsing True String Source)
   ============================================================================== *)
let source_code = "
' This is a native code comment. The lexer will completely drop it!
Public Structure Player
    Public ID As Integer
    Public FixedName(31) As Byte
End Structure

' Test Function 
Function CalculateDamage(baseDmg As Integer) As Integer
  Dim multiplier As Integer = 2
  Return baseDmg * multiplier
End Function
"

let () =
  print_endline "==================================================";
  print_endline "  Running OCaml String Lexer -> Parser Pipeline  " ;
  print_endline "==================================================";
try(* 1. Lex the raw source string down into individual token structures *)
let token_list = lex source_code in 
    print_endline "✓ Lexing Phase: SUCCESS";

(* 2. Feed token stream right into our state machine *)
let state = { tokens = token_list } in
let abstract_syntax_tree = parse_program state in 
    print_endline "✓ Parsing Phase: SUCCESS\n";

(* 3. Output validation summary *)
print_ast abstract_syntax_tree;
print_endline "==================================================";
print_endline "✓ End-to-End Execution Complete" 
    with
    | LexError msg -> Printf.eprintf "❌ Lexer Error: %s\n" msg; exit 1
  | ParseError msg -> Printf.eprintf "❌ Parser Error: %s\n" msg; exit 1
