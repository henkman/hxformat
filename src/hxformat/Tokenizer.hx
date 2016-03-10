package hxformat;
import hxformat.Tokenizer.Token;

enum TokenType {
	EOF;
	NEWLINE;
	COMMENT;

	COMMA;
	COLON;
	SEMICOLON;
	LBRACE;
	RBRACE;
	LPAREN;
	RPAREN;
	LBRACKET;
	RBRACKET;
	LESSTHAN;
	GREATERTHAN;
	PLUS;
	MINUS;
	STAR;
	EQUALS;
	SLASH;
	DOT;
	EXCLAMATION;
	QUESTION;
	TILDE;
	AND;
	PIPE;
	PERCENT;
	CARET;
	HASH;
	AT;

	INT;
	FLOAT;
	STRING;
	IDENTIFIER;

	ABSTRACT;
	BREAK;
	CASE;
	CAST;
	CATCH;
	CLASS;
	CONTINUE;
	DEFAULT;
	DO;
	DYNAMIC;
	ELSE;
	ENUM;
	EXTENDS;
	EXTERN;
	FALSE;
	FOR;
	FUNCTION;
	IF;
	IMPLEMENTS;
	IMPORT;
	IN;
	INLINE;
	INTERFACE;
	MACRO;
	NEVER;
	NEW;
	NULL;
	OVERRIDE;
	PACKAGE;
	PRIVATE;
	PUBLIC;
	RETURN;
	STATIC;
	SUPER;
	SWITCH;
	THIS;
	THROW;
	TRACE;
	TRUE;
	TRY;
	TYPEDEF;
	UNTYPED;
	USING;
	VAR;
	WHILE;

	UNEXPECTED;
}

class Token {
	public var type:TokenType;
	public var value:String;
	public function new(type:TokenType, ?value:String = null)
	{
		this.type = type;
		this.value = value;
	}

	private static var tokens:Map<TokenType, String>= [
	TokenType.NEWLINE =>"\n",
	TokenType.SEMICOLON =>";",
	TokenType.COLON =>":",
	TokenType.COMMA =>",",
	TokenType.LBRACE =>"{",
	TokenType.RBRACE =>"}",
	TokenType.LPAREN =>"(",
	TokenType.RPAREN =>")",
	TokenType.LBRACKET =>"[",
	TokenType.RBRACKET =>"]",
	TokenType.LESSTHAN =>"<",
	TokenType.GREATERTHAN =>">",
	TokenType.PLUS =>"+",
	TokenType.MINUS =>"-",
	TokenType.STAR =>"*",
	TokenType.EQUALS =>"=",
	TokenType.SLASH =>"/",
	TokenType.DOT =>".",
	TokenType.EXCLAMATION =>"!",
	TokenType.QUESTION =>"?",
	TokenType.TILDE =>"~",
	TokenType.AND =>"&",
	TokenType.PIPE =>"|",
	TokenType.PERCENT =>"%",
	TokenType.CARET =>"^",
	TokenType.HASH =>"#",
	TokenType.AT =>"@",
	TokenType.BREAK =>"break",
	TokenType.CASE =>"case",
	TokenType.CAST =>"cast",
	TokenType.CATCH =>"catch",
	TokenType.CLASS =>"class",
	TokenType.CONTINUE =>"continue",
	TokenType.DEFAULT =>"default",
	TokenType.DO =>"do",
	TokenType.DYNAMIC =>"dynamic",
	TokenType.ELSE =>"else",
	TokenType.ENUM =>"enum",
	TokenType.EXTENDS =>"extends",
	TokenType.EXTERN =>"extern",
	TokenType.FALSE =>"false",
	TokenType.FOR =>"for",
	TokenType.FUNCTION =>"function",
	TokenType.IF =>"if",
	TokenType.IMPLEMENTS =>"implements",
	TokenType.IMPORT =>"import",
	TokenType.IN =>"in",
	TokenType.INLINE =>"inline",
	TokenType.INTERFACE =>"interface",
	TokenType.NEVER =>"never",
	TokenType.NEW =>"new",
	TokenType.NULL =>"null",
	TokenType.OVERRIDE =>"override",
	TokenType.PACKAGE =>"package",
	TokenType.PRIVATE =>"private",
	TokenType.PUBLIC =>"public",
	TokenType.RETURN =>"return",
	TokenType.STATIC =>"static",
	TokenType.SUPER =>"super",
	TokenType.SWITCH =>"switch",
	TokenType.THIS =>"this",
	TokenType.THROW =>"throw",
	TokenType.TRACE =>"trace",
	TokenType.TRUE =>"true",
	TokenType.TRY =>"try",
	TokenType.TYPEDEF =>"typedef",
	TokenType.UNTYPED =>"untyped",
	TokenType.USING =>"using",
	TokenType.VAR =>"var",
	TokenType.WHILE =>"while"
	];

	public inline function typeString():String
	{
		return tokens.get(type);
	}

	public function isKeyword():Bool
	{
		return type == TokenType.ABSTRACT||type == TokenType.BREAK||
		type == TokenType.CASE||type == TokenType.CAST||
		type == TokenType.CATCH||type == TokenType.CLASS||
		type == TokenType.CONTINUE||type == TokenType.DEFAULT||
		type == TokenType.DO||type == TokenType.DYNAMIC||
		type == TokenType.ELSE||type == TokenType.ENUM||
		type == TokenType.EXTENDS||type == TokenType.EXTERN||
		type == TokenType.FALSE||type == TokenType.FOR||
		type == TokenType.FUNCTION||type == TokenType.IF||
		type == TokenType.IMPLEMENTS||type == TokenType.IMPORT||
		type == TokenType.IN||type == TokenType.INLINE||
		type == TokenType.INTERFACE||type == TokenType.MACRO||
		type == TokenType.NEW||type == TokenType.NULL||
		type == TokenType.OVERRIDE||type == TokenType.PACKAGE||
		type == TokenType.PRIVATE||type == TokenType.PUBLIC||
		type == TokenType.RETURN||type == TokenType.STATIC||
		type == TokenType.SWITCH||type == TokenType.THIS||
		type == TokenType.THROW||type == TokenType.TRUE||
		type == TokenType.TRY||type == TokenType.TYPEDEF||
		type == TokenType.UNTYPED||type == TokenType.USING||
		type == TokenType.VAR||type == TokenType.WHILE;
	}

	public function isSymbol():Bool
	{
		return type == TokenType.SEMICOLON||type == TokenType.COLON||
		type == TokenType.COMMA||type == TokenType.LBRACE||
		type == TokenType.RBRACE||type == TokenType.LPAREN||
		type == TokenType.RPAREN||type == TokenType.LBRACKET||
		type == TokenType.RBRACKET||type == TokenType.LESSTHAN||
		type == TokenType.GREATERTHAN||type == TokenType.PLUS||
		type == TokenType.MINUS||type == TokenType.STAR||
		type == TokenType.EQUALS||type == TokenType.SLASH||
		type == TokenType.DOT||type == TokenType.EXCLAMATION||
		type == TokenType.QUESTION||type == TokenType.TILDE||
		type == TokenType.AND||type == TokenType.PIPE||
		type == TokenType.PERCENT||type == TokenType.CARET||
		type == TokenType.HASH||type == TokenType.AT;
	}

	public function isBeginningOfComparison():Bool {
		return type == TokenType.EQUALS||type == TokenType.EXCLAMATION||
		type == TokenType.AND||type == TokenType.PIPE;
	}

	public inline function toString():String
	{
		if (value == null) {
			return typeString();
		}
		return value;
	}
}

class Tokenizer
{
	private var content:String;
	private var cur:UInt;
	private var marker:UInt;

	public function new(content:String)
	{
		this.content = content;
		cur = 0;
	}

	private inline function peek():Int
	{
		if (cur>= content.length) {
			return -1;
		}
		var c = StringTools.fastCodeAt(content, cur);
		return c;
	}

	private inline function next()
	{
		cur ++;
	}

	private inline function mark()
	{
		marker = cur;
	}

	private inline function marked():String
	{
		return content.substring(marker, cur);
	}

	private inline function isWhitespace(c:Int):Bool
	{
		return c == " ".code||c == "\t".code||c == "\r".code;
	}

	private inline function isNumber(c:Int):Bool
	{
		return c>= "0".code&&c<= "9".code;
	}

	private inline function isLowerAlpha(c:Int):Bool
	{
		return c>= "a".code&&c<= "z".code;
	}

	private inline function isUpperAlpha(c:Int):Bool
	{
		return c>= "A".code&&c<= "Z".code;
	}

	private inline function isAlpha(c:Int):Bool
	{
		return isLowerAlpha(c)||isUpperAlpha(c);
	}

	private function skipWhitespacePeek():Int
	{
		var c = peek();
		while (isWhitespace(c)) {
			next();
			c = peek();
		}
		return c;
	}

	// INFO: this is not correct. it detects number-like things only
	private function number(c:Int):Token
	{
		mark();
		var startsWithDot = c == ".".code;
		next();
		c = peek();
		if (startsWithDot&&!isNumber(c)) {
			return new Token(TokenType.DOT);
		}
		while (isNumber(c)||c == ".".code||isAlpha(c)) {
			next();
			c = peek();
		}
		return new Token(TokenType.INT, marked());
	}

	private function string(c:Int):Token
	{
		mark();
		var end = c == "\"".code ?"\"".code:"'".code;
		var ignoreNext = false;
		do {
			ignoreNext = !ignoreNext&&c == "\\".code;
			next();
			c = peek();
		} while (c != -1&&!(!ignoreNext&&c == end));
		next();
		return new Token(TokenType.STRING, marked());
	}

	private function comment(c:Int):Token
	{
		mark();
		next();
		c = peek();
		if (c == "/".code) {
			do {
				next();
				c = peek();
			} while (c != -1&&c != "\n".code);
			return new Token(TokenType.COMMENT, marked());
		} else if (c == "*".code) {
			var last:Int;
			do {
				next();
				last = c;
				c = peek();
			} while (c != -1&&!(last == "*".code&&c == "/".code));
			return new Token(TokenType.COMMENT, marked());
		}
		return new Token(TokenType.SLASH);
	}

	// INFO: this does not match the 0x7F to 0xFF range
	private function identifier(c:Int):Token
	{
		// [a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*
		mark();
		next();
		c = peek();
		while (c == "_".code||isAlpha(c)||isNumber(c)) {
			next();
			c = peek();
		}
		var str = marked();
		var type = switch (str) {
			case "abstract":TokenType.ABSTRACT;
			case "break":TokenType.BREAK;
			case "case":TokenType.CASE;
			case "cast":TokenType.CAST;
			case "catch":TokenType.CATCH;
			case "class":TokenType.CLASS;
			case "continue":TokenType.CONTINUE;
			case "default":TokenType.DEFAULT;
			case "do":TokenType.DO;
			case "dynamic":TokenType.DYNAMIC;
			case "else":TokenType.ELSE;
			case "enum":TokenType.ENUM;
			case "extends":TokenType.EXTENDS;
			case "extern":TokenType.EXTERN;
			case "false":TokenType.FALSE;
			case "for":TokenType.FOR;
			case "function":TokenType.FUNCTION;
			case "if":TokenType.IF;
			case "implements":TokenType.IMPLEMENTS;
			case "import":TokenType.IMPORT;
			case "in":TokenType.IN;
			case "inline":TokenType.INLINE;
			case "interface":TokenType.INTERFACE;
			case "macro":TokenType.MACRO;
			case "never":TokenType.NEVER;
			case "new":TokenType.NEW;
			case "null":TokenType.NULL;
			case "override":TokenType.OVERRIDE;
			case "package":TokenType.PACKAGE;
			case "private":TokenType.PRIVATE;
			case "public":TokenType.PUBLIC;
			case "return":TokenType.RETURN;
			case "static":TokenType.STATIC;
			case "super":TokenType.SUPER;
			case "switch":TokenType.SWITCH;
			case "this":TokenType.THIS;
			case "throw":TokenType.THROW;
			case "trace":TokenType.TRACE;
			case "true":TokenType.TRUE;
			case "try":TokenType.TRY;
			case "typedef":TokenType.TYPEDEF;
			case "untyped":TokenType.UNTYPED;
			case "using":TokenType.USING;
			case "var":TokenType.VAR;
			case "while":TokenType.WHILE;
			case _:TokenType.IDENTIFIER;
		} ;
		var tok = new Token(type);
		if (type == TokenType.IDENTIFIER)
		tok.value = str;
		return tok;
	}

	public function token():Token
	{
		var c = skipWhitespacePeek();
		switch (c) {
			case -1:return new Token(TokenType.EOF);
			case "\n".code:next();return new Token(TokenType.NEWLINE);
			case ";".code:next();return new Token(TokenType.SEMICOLON);
			case ":".code:next();return new Token(TokenType.COLON);
			case ",".code:next();return new Token(TokenType.COMMA);
			case "{".code:next();return new Token(TokenType.LBRACE);
			case "}".code:next();return new Token(TokenType.RBRACE);
			case "(".code:next();return new Token(TokenType.LPAREN);
			case ")".code:next();return new Token(TokenType.RPAREN);
			case "[".code:next();return new Token(TokenType.LBRACKET);
			case "]".code:next();return new Token(TokenType.RBRACKET);
			case "<".code:next();return new Token(TokenType.LESSTHAN);
			case ">".code:next();return new Token(TokenType.GREATERTHAN);
			case "+".code:next();return new Token(TokenType.PLUS);
			case "-".code:next();return new Token(TokenType.MINUS);
			case "*".code:next();return new Token(TokenType.STAR);
			case "=".code:next();return new Token(TokenType.EQUALS);
			case "!".code:next();return new Token(TokenType.EXCLAMATION);
			case "?".code:next();return new Token(TokenType.QUESTION);
			case "~".code:next();return new Token(TokenType.TILDE);
			case "&".code:next();return new Token(TokenType.AND);
			case "|".code:next();return new Token(TokenType.PIPE);
			case "%".code:next();return new Token(TokenType.PERCENT);
			case "^".code:next();return new Token(TokenType.CARET);
			case "#".code:next();return new Token(TokenType.HASH);
			case "@".code:next();return new Token(TokenType.AT);
			case "/".code:
			return comment(c);
			case "\"".code, "'".code:
			return string(c);
			case "0".code, "1".code, "2".code,
			"3".code, "4".code, "5".code,
			"6".code, "7".code, "8".code,
			"9".code, ".".code:
			return number(c);
			case "_".code,
			"a".code, "b".code, "c".code, "d".code,
			"e".code, "f".code, "g".code, "h".code,
			"i".code, "j".code, "k".code, "l".code,
			"m".code, "n".code, "o".code, "p".code,
			"q".code, "r".code, "s".code, "t".code,
			"u".code, "v".code, "w".code, "x".code,
			"y".code, "z".code, "A".code, "B".code,
			"C".code, "D".code, "E".code, "F".code,
			"G".code, "H".code, "I".code, "J".code,
			"K".code, "L".code, "M".code, "N".code,
			"O".code, "P".code, "Q".code, "R".code,
			"S".code, "T".code, "U".code, "V".code,
			"W".code, "X".code, "Y".code, "Z".code:
			return identifier(c);
			case _:
			return new Token(TokenType.UNEXPECTED, String.fromCharCode(c));
		} ;
	}
} 