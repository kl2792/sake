{ type token =  }

let spc = [' ' '\n' '\t']
let cap = ['A'-'Z']
let low = ['a'-'z']
let ltr = (cap | low)
let dgt = ['0'-'9']
let aln = (ltr | dgt)

rule token =
    parse spc { token lexbuf }
        | cap aln* { Enum }
        | low aln* { Name }
        | dgt+'.'dgt* { Float }
        | dgt+ { Int }
        | 
        | "(~"     { comment lexbuf }
and comment =
    parse "~)" { token lexbuf }
        | _    { comment lexbuf }
and line_comment =
    parse '\n' { token lexbuf }
        | _    { line_comment lexbuf }
