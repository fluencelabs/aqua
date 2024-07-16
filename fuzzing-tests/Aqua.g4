grammar Aqua;

tokens {
	INDENT,
	DEDENT,
	NL
}

@lexer::members {
# A hack to support indentation
def create_node(self, node, *args, **kwargs):
    result = super().create_node(node, *args, **kwargs)

    if not hasattr(self, "indent_level"):
        self.indent_level = 0

    if node.name == "INDENT": 
        self.indent_level += 1
        result.src = "\n" + "  " * self.indent_level
    elif node.name == "DEDENT":
        self.indent_level -= 1
        result.src = "\n" + "  " * self.indent_level
    elif node.name == "NL":
        result.src = "\n" + "  " * self.indent_level
    
    return result
}

prog: function+;

function: FUNC SP+ closure;

closure:
	ID LPAREN ((typedId COMMA SP*)* typedId)? RPAREN COLON block;

block: INDENT ('expr' NL | ifStat)+ DEDENT;

ifStat: IF SP+ ID COLON block;

typedId: ID SP* COLON SP* type;

basicType:
	'⊥'
	| '⊤'
	| 'u8'
	| 'u16'
	| 'u32'
	| 'u64'
	| 'i8'
	| 'i16'
	| 'i32'
	| 'i64'
	| 'f32'
	| 'f64'
	| 'bool'
	| 'string';

namedType: ID;

dataType:
	basicType
	| namedType
	| ARRAY SP* dataType
	| OPTION SP* dataType
	| STREAM SP* dataType;

arrowTypeAbilities_aux:
	LBRACE SP* namedType SP* (COMMA SP* namedType)* SP* RBRACE;

// Only data types are allowed as arguments for arrow type
arrowTypeArgs_aux: (dataType SP* (COMMA SP* dataType)*)?;

arrowTypeRet_aux:
	typeParen_aux SP* (COMMA SP* typeParen_aux)*
	| LPAREN RPAREN; // for no return

arrowType:
	arrowTypeAbilities_aux SP* arrowTypeArgs_aux SP* RARROW SP* arrowTypeRet_aux;

type: dataType | arrowType;

typeParen_aux: LPAREN SP* type SP* RPAREN | type;

ARRAY: '[]';
OPTION: '?';
STREAM: '*';

IF: 'if';
FUNC: 'func';

RARROW: '->';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
COLON: ':';
COMMA: ',';
SP: ' ';

ID: [a-zA-Z][a-zA-Z_]+;
