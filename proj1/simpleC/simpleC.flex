import java_cup.runtime.*;

%%

%public
%class Scanner
%implements sym

%line
%column

%cup
%cupdebug

%{
  StringBuffer string = new StringBuffer();
  
  private Symbol symbol(int type) {
    return new MySymbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol(int type, Object value) {
    return new MySymbol(type, yyline+1, yycolumn+1, value);
  }
%}

/* White spaces */
WhiteSpace = [ \t\f\r\n]

/* Comments in C files: no need to worry about them: assume the C
 * preprocessor was already executed - it strips away comments 
 */

/* Identifiers: TODO - make them general, as described in Section
 * 6.4.2.1 in the ANSI C document. Ignore "universal-character-name"
 * and "other implementation-defined characters" mentioned in Section
 * 6.4.2.1.
 */

/* Replace this placeholder with your own definitions */
Identifier = [a-z]

/* Integer literals: TODO - handle integer literals as described in
 * Section 6.4.4.1 of the ANSI C document. For simplicity, do NOT
 * handle the 'long long' cases. But DO handle long, unsigned,
 * hexadecimal, and octal constants.
 */

/* Replace this placeholder with your own definitions */
DecIntegerLiteral = 0 | [1-9][0-9]*

/* Floating point literals: TODO - handle floating point literals as 
 * described in Section 6.4.4.2 of the ANSI C document. For
 * simplicity, do NOT handle the 'long double' cases. But DO handle
 * hexadecimal floating constants, e/E and p/P notation, and f/F
 * suffixes.
 */        

/* Replace this placeholder with your own definitions */
DoubleLiteral  = [0-9]+ \. [0-9]* 

%%

<YYINITIAL> {

  /* Keywords: TODO - add all keywords from Section 6.4.1 */

  "auto"                         { return symbol(AUTO); }
  "break"                        { return symbol(BREAK); }
  "case"                         { return symbol(CASE); }
  "char"                         { return symbol(CHAR); }
  "const"                        { return symbol(CONST); }
  "continue"                     { return symbol(CONTINUE); }
  "default"                      { return symbol(DEFAULT); }
  "do"                           { return symbol(DO); }
  "double"                       { return symbol(DOUBLE); }
  "else"                         { return symbol(ELSE); }
  "enum"                         { return symbol(ENUM); }
  "extern"                       { return symbol(EXTERN); }
  "float"                        { return symbol(FLOAT); }
  "for"                          { return symbol(FOR); }
  "goto"                         { return symbol(GOTO); }
  "if"                           { return symbol(IF); }
  "inline"                       { return symbol(INLINE); }
  "int"                          { return symbol(INT); }
  "long"                         { return symbol(LONG); }
  "register"                     { return symbol(REGISTER); }
  "return"                       { return symbol(RETURN); }
  "short"                        { return symbol(SHORT); }
  "signed"                       { return symbol(SIGNED); }
  "sizeof"                       { return symbol(SIZEOF); }
  "static"                       { return symbol(STATIC); }
  "struct"                       { return symbol(STRUCT); }
  "switch"                       { return symbol(SWITCH); }
  "typedef"                      { return symbol(TYPEDEF); }
  "union"                        { return symbol(UNION); }
  "unsigned"                     { return symbol(UNSIGNED); }
  "void"                         { return symbol(VOID); }
  "volatile"                     { return symbol(VOLATILE); }
  "while"                        { return symbol(WHILE); }
  "_Bool"                        { return symbol(_BOOL); }
  "_Complex"                     { return symbol(_COMPLEX); }
  "_Imaginary"                   { return symbol(_IMAGINARY); }

  /* Punctuators: TODO - add all punctuators from Section 6.4.6 except
  for the last eight punctuators */

  "<<="                          { return symbol(LSHIFTEQUAL); }
  ">>="                          { return symbol(RSHIFTEQUAL); }
  "..."                          { return symbol(ELLIPSES); }
  "*="                           { return symbol(STAREQUAL); }
  "/="                           { return symbol(DIVEQUAL); }
  "%="                           { return symbol(MODEQUAL); }
  "+="                           { return symbol(PLUSEQUAL); }
  "-="                           { return symbol(MINUSEQUAL); }
  "++"                           { return symbol(PLUSPLUS); }
  "--"                           { return symbol(MINUSMINUS); }
  "=="                           { return symbol(EQUALEQUAL); }
  "->"                           { return symbol(ARROW); }
  "<<"                           { return symbol(LSHIFT); }
  ">>"                           { return symbol(RSHIFT); }
  "<="                           { return symbol(LESSEQUAL); }
  ">="                           { return symbol(GREATEREQUAL); }
  "&="                           { return symbol(BITANDEQUAL); }
  "|="                           { return symbol(BITOREQUAL); }
  "^="                           { return symbol(BITXOREQUAL); }
  "!="                           { return symbol(NOTEQUAL); }
  "&&"                           { return symbol(AND); }
  "||"                           { return symbol(OR); }
  "##"                           { return symbol(SHARPSHARP); }
  "("                            { return symbol(LPAREN); }
  ")"                            { return symbol(RPAREN); }
  "{"                            { return symbol(LBRACE); }
  "}"                            { return symbol(RBRACE); }
  "["                            { return symbol(LBRACK); }
  "]"                            { return symbol(RBRACK); }
  "&"                            { return symbol(BITAND); }
  "*"                            { return symbol(STAR); }
  "+"                            { return symbol(PLUS); }
  "-"                            { return symbol(MINUS); }
  "~"                            { return symbol(TILDE); }
  "/"                            { return symbol(DIV); }
  "%"                            { return symbol(MOD); }
  "!"                            { return symbol(NOT); }
  "."                            { return symbol(DOT); }
  ","                            { return symbol(COMMA); }
  "="                            { return symbol(ASSGN); }
  "<"                            { return symbol(LESS); }
  ">"                            { return symbol(GREATER); }
  "|"                            { return symbol(BITOR); }
  "^"                            { return symbol(BITXOR); }
  "?"                            { return symbol(QUESTION); }
  ":"                            { return symbol(COLON); }
  ";"                            { return symbol(SEMICOLON); }
  "#"                            { return symbol(SHARP); }

  /* Integer literals: TODO - for any such literal, the token type
   * should be INTEGER_LITERAL, as shown below. The attribute value
   * should be either a Java Integer object (for values that are not
   * 'long') or a Java Long object (for values that are 'long'). You
   * can assume that the value of the literal is always small enough
   * to fit in a Java Integer or Long, respectively. For example, if
   * the literal is "17", "0x11", or "021", you should create a Java
   * Integer object containing the Java 'int' value 17. 
   */

  /* replace this placeholder with your own definitions */
  {DecIntegerLiteral}            { return symbol(INTEGER_LITERAL, new Integer(yytext())); }

  /* Floating-point literals: TODO - for any such literal, the token
   * type should be FLOATING_POINT_LITERAL, as shown below. The
   * attribute value should be either a Java Float object (for values
   * that are 'float') or a Java Double object (for values that are
   * 'double'). You can assume that the value of the literal is always
   * small enough to fit in a Java Float or Double, respectively. For
   * example, if the literal is ".5625f", "5625e-4f", or "0X1.2p-1f",
   * you should create a Java Float object containing the Java 'float'
   * value 0.5625.
   */

  /* replace this placeholder with your own definitions */
  {DoubleLiteral}                { return symbol(FLOATING_POINT_LITERAL, new Double(yytext())); }
  
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */ 
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); }  
}

/* error fallback */
.|\n                             { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+(yyline+1)+", column "+(yycolumn+1)); }
<<EOF>>                          { return symbol(EOF); }
