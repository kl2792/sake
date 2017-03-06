{ type token =  }

let spc = [' ' '\t']
let cap = ['A'-'Z']
let low = ['a'-'z']
let ltr = (cap | low)
let dgt = ['0'-'9']
let aln = (ltr | dgt)

rule token = parse
    spc { token lexbuf }
  | "(~" { comment lexbuf }
  | "~" { line_comment lexbuf }
  | cap aln* { ENUM }
  | low aln* { VARIABLE }
  | dgt+'.'dgt*|'.'dgt+ { FLOATLIT }
  | dgt+ { INTLIT }
  | "fsm" { FSM }
  | "type" { TYPE }
  | "switch" { CASE }
  | "goto" { GOTO }
  | "state" { STATE }
  | "type" { TYPE }
  | "switch" { SWITCH } 
  | "case" { CASE } 
  | "goto" { GOTO }
  | "fsm"  { FSM }
  | "state" { STATE }
  | "start" { START }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | "sysin" { SYSIN }
and comment = parse
    "~)" { token lexbuf }
  | _    { comment lexbuf }
and line_comment = parse
    '\n' { token lexbuf }
  | _    { line_comment lexbuf }
