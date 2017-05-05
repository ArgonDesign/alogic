parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start : (entities+=entity)* EOF ;

entity : 
   typedef
 | define
 | task
 ;
  
typedef : TYPEDEF known_type IDENTIFIER SEMICOLON;

define : HASHDEFINE IDENTIFIER expr; // TODO May want to stop at end of line somehow?
  
tasktype : FSM | PIPELINE | VERILOG | NETWORK;

initializer : EQUALS expr;

declaration : known_type primary_expr initializer?;

task_declaration : 
    OUT SYNC_TYPE? known_type IDENTIFIER SEMICOLON     #OutDecl
  | IN SYNC_TYPE? known_type IDENTIFIER SEMICOLON      #InDecl
  | CONST SYNC_TYPE? known_type IDENTIFIER initializer? SEMICOLON   #ConstDecl
  | VERILOG SYNC_TYPE? known_type primary_expr SEMICOLON #VerilogDecl
  | declaration SEMICOLON                              #Decl
  ;
    
task : tasktype IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=task_content)* RIGHTCURLY;

known_type : 
  BOOL
  | INTTYPE
  | UINTTYPE
  | IDENTIFIER
  | STRUCT LEFTCURLY (fields+=field)* RIGHTCURLY
  | INT LEFTBRACKET expr RIGHTBRACKET
  | UINT LEFTBRACKET expr RIGHTBRACKET
  ;
    
task_content :
  VOID IDENTIFIER LEFTBRACKET RIGHTBRACKET statement
  | VOID FENCE LEFTBRACKET RIGHTBRACKET statement
  | VERILOGBODY verilogbody VRIGHTCURLY
  ;
  
verilogbody : (tks+=verilogtoken)*;

verilogtoken :
  VANY
  | VLEFTCURLY verilogbody VRIGHTCURLY
  ;
  
expr : binary_expr # NotTernaryExpr
     | binary_expr QUESTIONMARK expr COLON expr # TernaryExpr
     ; 
      
binary_op : BINARYOP | AND | OR | MINUS;
   
binary_expr : 
  unary_expr binary_op expr # BinaryExpr
  | unary_expr # NotBinaryExpr
  ;
    
unary_op : NOT | TILDA | OR | MINUS | AND;
        
unary_expr : 
  unary_op primary_expr
  | primary_expr 
  ;
    
primary_expr : 
  secondary_expr LEFTSQUARE expr RIGHTSQUARE
  | secondary_expr LEFTSQUARE expr arrayop expr RIGHTSQUARE
  | secondary_expr
  ;
    
arrayop : 
  COLON
  | MINUSCOLON
  | PLUSCOLON
  ;
  
comma_args : (es+=expr)? (COMMA es+=expr)*;
    
secondary_expr : 
  TRUE
  | FALSE
  | LEFTBRACKET expr RIGHTBRACKET
  | TICKNUM
  | CONSTANT TICKNUM
  | CONSTANT
  | LITERAL 
  | LEFTCURLY expr LEFTCURLY expr RIGHTCURLY RIGHTCURLY
  | LEFTCURLY comma_args RIGHTCURLY
  | dotted_name LEFTBRACKET comma_args RIGHTBRACKET
  | DOLLAR LEFTBRACKET comma_args RIGHTBRACKET
  | IDENTIFIER TICKNUM // Used to handle #define for the NUM'd0
  | dotted_name
  ;
   
dotted_name : (es+=IDENTIFIER) (DOT es+=IDENTIFIER)*;

field : known_type IDENTIFIER SEMICOLON;
       
assign_op : EQUALS | ASSIGNOP;
      
case_stmt : 
  DEFAULT COLON statement 
  | comma_args COLON statement 
  ;
    
else_statement : ELSE statement;

statement : 
  LEFTCURLY (stmts+=statement)* RIGHTCURLY  # BlockStmt
  | declaration # DeclStmt
  | WHILE LEFTBRACKET expr RIGHTBRACKET statement # WhileStmt
  | IF LEFTBRACKET expr RIGHTBRACKET statement else_statement? # IfStmt
  | CASE LEFTBRACKET expr RIGHTBRACKET LEFTCURLY (cases+=case_stmt)+ RIGHTCURLY # CaseSTmt
  | FOR LEFTBRACKET single_statement SEMICOLON expr SEMICOLON single_statement RIGHTBRACKET LEFTCURLY (stmts += statement)* RIGHTCURLY # ForStmt
  | DO LEFTCURLY (stmts += statement)* RIGHTCURLY WHILE LEFTBRACKET expr RIGHTBRACKET SEMICOLON  # DoStmt
  | single_statement SEMICOLON # SingleStmt
  ;
    
single_statement : 
  |primary_expr PLUSPLUS
  |primary_expr MINUSMINUS
  |primary_expr assign_op expr
  |primary_expr
  |FENCE 
  |BREAK 
  |RETURN
  |DOLLARCOMMENT LEFTBRACKET LITERAL RIGHTBRACKET
  |GOTO IDENTIFIER
  ;


