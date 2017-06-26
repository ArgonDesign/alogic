parser grammar VParser;

options {
  tokenVocab = VLexer;
}

start : (entities+=entity)* EOF ;

entity :
   typedef
 | task
 | network
 ;

typedef : TYPEDEF known_type IDENTIFIER SEMICOLON;

tasktype : FSM #FsmType
  | PIPELINE   #PipelineType
  | VERILOG    #VerilogType
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
  | PARAM sync_type? known_type IDENTIFIER initializer? SEMICOLON   #ParamDecl
  | VERILOG known_type primary_expr SEMICOLON #VerilogDecl
  | declaration SEMICOLON                              #Decl
  ;

task : tasktype IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=task_content)* RIGHTCURLY;

network : NETWORK IDENTIFIER LEFTCURLY (decls+=task_declaration)* (contents+=network_content)* RIGHTCURLY;

network_content : task | connect | instantiate;

connect : dotted_name GOESTO comma_args SEMICOLON;

instantiate : IDENTIFIER EQUALS IDENTIFIER LEFTBRACKET param_args RIGHTBRACKET SEMICOLON;

known_type :
  BOOL                                            # BoolType
  | INTTYPE                                       # IntType
  | UINTTYPE                                      # UintType
  | IDENTIFIER                                    # IdentifierType
  | STRUCT LEFTCURLY (fields+=field)* RIGHTCURLY  # StructType
  | INT LEFTBRACKET comma_args RIGHTBRACKET       # IntVType
  | UINT LEFTBRACKET comma_args RIGHTBRACKET      # UintVType
  ;

task_content :
  VOID IDENTIFIER LEFTBRACKET RIGHTBRACKET statement  # Function
  | VOID FENCE LEFTBRACKET RIGHTBRACKET statement     # FenceFunction
  | VERILOGBODY verilogbody VRIGHTCURLY               # VerilogFunction
  ;

verilogbody : (tks+=verilogtoken)*;

verilogtoken :
  VANY  # Vany
  | VLEFTCURLY verilogbody VRIGHTCURLY # Vbody
  ;

// TODO: Express expr as a single left-recursive structure
//       Antlr will recognize this and treat with appropriate precedence

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

param_args : (es+=paramAssign)? (COMMA es+=paramAssign)*;

paramAssign : expr EQUALS expr;

secondary_expr :
  TRUE # TrueExpr
  | FALSE # FalseExpr
  | LEFTBRACKET expr RIGHTBRACKET # BracketExpr
  | TICKNUM # TicknumExpr
  | CONSTANT TICKNUM # ConstantTickNumExpr
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


case_stmt :
  DEFAULT COLON statement # DefaultCase
  | comma_args COLON statement # NormalCase
  ;

statement
  : LEFTCURLY (stmts+=statement)* RIGHTCURLY                # BlockStmt
  | declaration ';'                                         # DeclStmt
  | 'while' '(' expr ')' statement                          # WhileStmt
  | 'if' '(' expr ')'
      thenStmt=statement
    ('else'
      elseStmt=statement)?                                  # IfStmt
  | 'case' '(' expr ')' LEFTCURLY
      (cases+=case_stmt)+
    RIGHTCURLY                                              # CaseStmt
  | 'for' '(' init=assignment_statement ';'
              cond=expr ';'
              step=assignment_statement ')' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY                                              # ForStmt
  | 'do' LEFTCURLY
      (stmts += statement)*
    RIGHTCURLY 'while' '(' expr ')' ';'                     # DoStmt
  | 'fence' ';'                                             # FenceStmt
  | 'break' ';'                                             # BreakStmt
  | 'return' ';'                                            # ReturnStmt
  | '$' '(' LITERAL ')' ';'                                 # DollarCommentStmt
  | 'goto' IDENTIFIER ';'                                   # GotoStmt
  | assignment_statement ';'                                # AssignmentStmt
  | primary_expr ';'                                        # ExprStmt
  ;

assign_op : '=' | ASSIGNOP;

assignment_statement
  : primary_expr '++'             # PrimaryIncStmt
  | primary_expr '--'             # PrimaryDecStmt
  | primary_expr assign_op expr   # AssignStmt
  ;


