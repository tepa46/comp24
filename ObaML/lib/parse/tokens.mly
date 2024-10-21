%token <int> INT
%token <string * (Lexing.position * Lexing.position)> STRING

%token <string> LOWERID
%token <string> UPPERID

%token UNDERSCORE                               "_"
%token LPAREN                                   "("
%token RPAREN                                   ")"
%token LBRACKET                                 "["
%token RBRACKET                                 "]"
%token ASTERISK (* also infix operator *)       "*"
%token COMMA                                    ","
%token ARROW (* also infix operator *)          "->"
%token QUOTE                                    "'"
%token BAR                                      "|"
%token COLON                                    ":"
%token COLONCOLON                               "::"
%token EQUAL                                    "="

%token OF                                       "of"
%token TYPE                                     "type"
%token TRUE                                     "true"
%token FALSE                                    "false"

%token EOF

%right ARROW

%%