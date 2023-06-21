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

type: 'u16';

LPAREN: '(';
RPAREN: ')';
COLON: ':';
COMMA: ',';
SP: ' ';
IF: 'if';
FUNC: 'func';
ID: [a-zA-Z][a-zA-Z_]+;
