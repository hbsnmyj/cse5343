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

  private Number parseFloatLiteral(String value, int base) {
    boolean isDouble = false;
    if(value.charAt(value.length() - 1) == 'l' ||
       value.charAt(value.length() - 1) == 'L') {
      isDouble = true;
      value = value.substring(0, value.length() - 1);
    } else if(value.charAt(value.length() - 1) == 'f' ||
       value.charAt(value.length() - 1) == 'F') {
      value = value.substring(0, value.length() - 1);
    }
    String sep;
    int dotPos = value.indexOf('.');
    int sepPos = 0;
    double intPart;
    double fracPart;
    double expPart;
    int baseExp = (base == 16) ? 2 : 10;
    if(base == 10) {
      sepPos = value.indexOf('e');
      if(sepPos== -1)
        sepPos = value.indexOf('E');
    } else if(base == 16) {
      sepPos = value.indexOf('p');
      if(sepPos== -1)
        sepPos = value.indexOf('P');
    }
    if(dotPos == -1) { /* xxxxExxxx or xxxxPxxxx */
      intPart = 0;
      String floatStr = value.substring(0, sepPos);
      fracPart = Integer.parseInt(floatStr, base) * Math.pow(base, -floatStr.length());
      expPart = Integer.parseInt(value.substring(sepPos + 1), base);
    } else if (sepPos == -1) { //xxxx.xxxx
      intPart = Integer.parseInt(value.substring(0, dotPos), base);
      String floatStr = value.substring(dotPos+1, value.length());
      fracPart = Integer.parseInt(floatStr, base) * Math.pow(base, -floatStr.length());
      expPart = 0;
    } else {
      if(dotPos != 0)
        intPart = Integer.parseInt(value.substring(0, dotPos), base);
      else
        intPart = 0;
      String floatStr = value.substring(dotPos + 1, sepPos);
      fracPart = Integer.parseInt(floatStr, base) * Math.pow(base, -floatStr.length());
      expPart = Integer.parseInt(value.substring(sepPos + 1), base);
    }
    Double result = (intPart + fracPart) * Math.pow(baseExp, expPart);
    if(isDouble)
      return new Double(result);
    else
      return new Float(result);
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

NonDigit = [a-zA-Z_]
Digit = [0-9]

/* Replace this placeholder with your own definitions */
Identifier = {NonDigit}({Digit}|{NonDigit})*

/* Integer literals: TODO - handle integer literals as described in
 * Section 6.4.4.1 of the ANSI C document. For simplicity, do NOT
 * handle the 'long long' cases. But DO handle long, unsigned,
 * hexadecimal, and octal constants.
 */

/* Replace this placeholder with your own definitions */
ULSuffix = ([Uu][Ll]|[Ll][Uu])

DecInt = 0 | [1-9]{Digit}*
DecIntegerLiteral = {DecInt}
DecLongLiteral = {DecInt}[Ll]
DecUnsignedLiteral ={DecInt}[Uu]
DecUnsignedLongLiteral ={DecInt}{ULSuffix}
OctInt = 0[1-7][0-7]*
OctIntegerLiteral = {OctInt}
OctLongLiteral = {OctInt}[Ll]
OctUnsignedLiteral ={OctInt}[Uu]
OctUnsignedLongLiteral ={OctInt}{ULSuffix}
HexInt = 0[xX][1-9A-F][0-9A-F]*
HexIntegerLiteral = {HexInt}
HexLongLiteral = {HexInt}[Ll]
HexUnsignedLiteral = {HexInt}[Uu]
HexUnsignedLongLiteral = {HexInt}{ULSuffix}


/* Floating point literals: TODO - handle floating point literals as 
 * described in Section 6.4.4.2 of the ANSI C document. For
 * simplicity, do NOT handle the 'long double' cases. But DO handle
 * hexadecimal floating constants, e/E and p/P notation, and f/F
 * suffixes.
 */        

/* Replace this placeholder with your own definitions */
ExponentPart = [eE]("+"|"-")?[0-9]+
FloatSuffix = [fF]
DecFractionalFloat = [0-9]*"."[0-9]+{ExponentPart}?{FloatSuffix}?
DecIntExponentFloat = [0-9]+{ExponentPart}{FloatSuffix}?
BinaryExponentPart = [pP]("+"|"-")?[01]+
HexFractionalFloat = 0[xX][1-9A-F]*"."[1-9A-F]{BinaryExponentPart}?{FloatSuffix}?
HexIntExponentFloat = 0[xX][1-9A-F]+{BinaryExponentPart}{FloatSuffix}?

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
/*  "##"                           { return symbol(SHARPSHARP); } */
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
/*  "#"                            { return symbol(SHARP); } */

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
  {DecLongLiteral}               { return symbol(LONG_LITERAL, new Long(yytext().substring(0, yytext().length() - 1))); }
  {DecUnsignedLiteral}               { return symbol(UNSIGNED_LITERAL, new Integer(yytext().substring(0, yytext().length() - 1))); }
  {DecUnsignedLongLiteral}               { return symbol(UNSIGNED_LITERAL, new Long(yytext().substring(0, yytext().length() - 2))); }
  {OctIntegerLiteral}            { return symbol(INTEGER_LITERAL, Integer.parseInt(yytext().substring(1, yytext().length()), 8)); }
  {OctLongLiteral}            { return symbol(LONG_LITERAL, Long.parseLong(yytext().substring(1, yytext().length()), 8)); }
  {OctUnsignedLiteral}            { return symbol(UNSIGNED_LITERAL, Integer.parseInt(yytext().substring(1, yytext().length() - 1), 8)); }
  {OctUnsignedLongLiteral}            { return symbol(UNSIGNED_LONG_LITERAL, Long.parseLong(yytext().substring(1, yytext().length() - 2), 8)); }
  {HexIntegerLiteral}            { return symbol(INTEGER_LITERAL, Integer.parseInt(yytext().substring(2, yytext().length()), 16)); }
  {HexLongLiteral}               { return symbol(LONG_LITERAL, Long.parseLong(yytext().substring(2, yytext().length() - 1), 16)); }
  {HexUnsignedLiteral}               { return symbol(UNSIGNED_LITERAL, Integer.parseInt(yytext().substring(2, yytext().length() - 1), 16)); }
  {HexUnsignedLongLiteral}               { return symbol(UNSIGNED_LITERAL, Long.parseLong(yytext().substring(2, yytext().length() - 2), 16)); }

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
  {DecFractionalFloat}           {
                                   return symbol(FLOATING_POINT_LITERAL,
                                       parseFloatLiteral(yytext(), 10));
                                 }
  {DecIntExponentFloat}             { return symbol(FLOATING_POINT_LITERAL, parseFloatLiteral(yytext(), 10)); }
  {HexFractionalFloat}           { return symbol(FLOATING_POINT_LITERAL, parseFloatLiteral(yytext().substring(2), 16)); }
  {HexIntExponentFloat}           { return symbol(FLOATING_POINT_LITERAL, parseFloatLiteral(yytext().substring(2), 16)); }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); }
}

/* error fallback */
.|\n                             { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+(yyline+1)+", column "+(yycolumn+1)); }
<<EOF>>                          { return symbol(EOF); }
