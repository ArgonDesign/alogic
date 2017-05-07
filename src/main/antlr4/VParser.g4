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
  
tasktype : FSM #FsmType
  | PIPELINE   #PipelineType
  | VERILOG    #VerilogType
  | NETWORK    #NetworkType
  ;

initializer : EQUALS expr;

declaration : known_type primary_expr initializer?;

sync_type : SYNC_READY_BUBBLE #SyncReadyBubbleType
          | WIRE_SYNC_ACCEPT  #WireSyncAcceptType
          | SYNC_READY #SyncReadyType
          | WIRE_SYNC #WireSyncType
          | SYNC_ACCEPT #SyncAcceptType
          | SYNC #SyncType
          | WIRE #WireType
          ;

task_declaration : 
    OUT sync_type? known_type IDENTIFIER SEMICOLON     #OutDecl
  | IN sync_type? known_type IDENTIFIER SEMICOLON      #InDecl
  | CONST sync_type? known_type IDENTIFIER initializer? SEMICOLON   #ConstDecl
  | VERILOG sync_type? known_type primary_expr SEMICOLON #VerilogDecl
  | declaration SEMICOLON                              #Decl
  ;
    
task : tasktype IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=task_content)* RIGHTCURLY;

known_type : 
  BOOL                                            # BoolType
  | INTTYPE                                       # IntType
  | UINTTYPE                                      # UintType
  | IDENTIFIER                                    # IdentifierType
  | STRUCT LEFTCURLY (fields+=field)* RIGHTCURLY  # StructType
  | INT LEFTBRACKET expr RIGHTBRACKET             # IntVType
  | UINT LEFTBRACKET expr RIGHTBRACKET            # UintVType
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
  unary_op primary_expr # UnaryExpr
  | primary_expr # NotUnaryExpr
  ;
    
primary_expr : 
  secondary_expr LEFTSQUARE expr RIGHTSQUARE  # ArrayAccessExpr
  | secondary_expr LEFTSQUARE expr arrayop expr RIGHTSQUARE # ArrayAccess2Expr
  | secondary_expr # SecondaryExpr
  ;
    
arrayop : 
  COLON
  | MINUSCOLON
  | PLUSCOLON
  ;
  
comma_args : (es+=expr)? (COMMA es+=expr)*;
    
secondary_expr : 
  TRUE # TrueExpr
  | FALSE # FalseExpr
  | LEFTBRACKET expr RIGHTBRACKET # BracketExpr
  | TICKNUM # TicknumExpr
  | CONSTANT TICKNUM # ConstantTickNumExpr
  | IDENTIFIER TICKNUM # IdentifierTickNumExpr // Used to handle #define for the NUM'd0
  | CONSTANT # ConstantExpr
  | LITERAL # LiteralExpr
  | LEFTCURLY expr LEFTCURLY expr RIGHTCURLY RIGHTCURLY # BitRepExpr
  | LEFTCURLY comma_args RIGHTCURLY # BitCatExpr
  | dotted_name LEFTBRACKET comma_args RIGHTBRACKET # FunCallExpr
  | DOLLAR LEFTBRACKET comma_args RIGHTBRACKET # DollarExpr
  | dotted_name # DottedNameExpr
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
  | declaration SEMICOLON # DeclStmt
  | WHILE LEFTBRACKET expr RIGHTBRACKET statement # WhileStmt
  | IF LEFTBRACKET expr RIGHTBRACKET statement else_statement? # IfStmt
  | CASE LEFTBRACKET expr RIGHTBRACKET LEFTCURLY (cases+=case_stmt)+ RIGHTCURLY # CaseSTmt
  | FOR LEFTBRACKET single_statement SEMICOLON expr SEMICOLON single_statement RIGHTBRACKET LEFTCURLY (stmts += statement)* RIGHTCURLY # ForStmt
  | DO LEFTCURLY (stmts += statement)* RIGHTCURLY WHILE LEFTBRACKET expr RIGHTBRACKET SEMICOLON  # DoStmt
  | single_statement SEMICOLON # SingleStmt
  ;
    
single_statement : 
  primary_expr PLUSPLUS   # PrimaryIncStmt
  |primary_expr MINUSMINUS # PrimaryDecStmt
  |primary_expr assign_op expr # AssignStmt
  |primary_expr            # PrimaryStmt
  |FENCE                   # FenceStmt
  |BREAK                   # BreakStmt
  |RETURN                  # ReturnStmt
  |DOLLARCOMMENT LEFTBRACKET LITERAL RIGHTBRACKET # DollarCommentStmt
  |GOTO IDENTIFIER         # GotoStmt
  ;


