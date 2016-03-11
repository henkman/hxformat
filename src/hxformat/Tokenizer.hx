package hxformat;
import hxformat.Tokenizer.Token;

enum TokenType
{
	EOF;
	UNEXPECTED;
	NEWLINE;
	COMMENT;
	IDENTIFIER;
	INT;
	FLOAT;
	STRING;

	// SINGLE SYMBOLS
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

	// COMPOUNDS SYMBOLS
	LOGIC_NOT_EQUALS;
	LOGIC_EQUALS;
	LOGIC_GREATER_THAN_EQUALS;
	LOGIC_LESS_THAN_EQUALS;
	LOGIC_AND;
	LOGIC_OR;
	SHIFT_LEFT;
	SHIFT_RIGHT;
	LOGIC_RIGHT_SHIFT;
	INCREMENT;
	DECREMENT;
	// ASSIGNMENT COMPOUND SYMBOLS
	PLUS_EQUALS;
	MINUS_EQUALS;
	STAR_EQUALS;
	SLASH_EQUALS;
	PERCENT_EQUALS;
	CARET_EQUALS;
	AND_EQUALS;
	PIPE_EQUALS;
	SHIFT_LEFT_EQUALS;
	SHIFT_RIGHT_EQUALS;
	LOGIC_RIGHT_SHIFT_EQUALS;
	EQUALS_GREATER_THAN;

	// KEYWORDS
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
}

class Token
{
	public var type:TokenType;
	public var value:String;
	public function new(type:TokenType, ?value:String = null)
	{
		this.type = type;
		this.value = value;
	}

	private static var tokens:Map<TokenType, String >= [
	TokenType.NEWLINE => "\n",
	TokenType.SEMICOLON => ";",
	TokenType.COLON => ":",
	TokenType.COMMA => ",",
	TokenType.LBRACE => "{",
	TokenType.RBRACE => "}",
	TokenType.LPAREN => "(",
	TokenType.RPAREN => ")",
	TokenType.LBRACKET => "[",
	TokenType.RBRACKET => "]",
	TokenType.LESSTHAN => "<",
	TokenType.GREATERTHAN => ">",
	TokenType.PLUS => "+",
	TokenType.MINUS => "-",
	TokenType.STAR => "*",
	TokenType.EQUALS => "=",
	TokenType.SLASH => "/",
	TokenType.DOT => ".",
	TokenType.EXCLAMATION => "!",
	TokenType.QUESTION => "?",
	TokenType.AND => "&",
	TokenType.PIPE => "|",
	TokenType.PERCENT => "%",
	TokenType.CARET => "^",
	TokenType.HASH => "#",
	TokenType.AT => "@",
	TokenType.LOGIC_NOT_EQUALS => "!=",
	TokenType.LOGIC_EQUALS => "==",
	TokenType.LOGIC_GREATER_THAN_EQUALS => ">=",
	TokenType.LOGIC_LESS_THAN_EQUALS => "<=",
	TokenType.LOGIC_AND => "&&",
	TokenType.LOGIC_OR => "||",
	TokenType.SHIFT_LEFT => "<<",
	TokenType.SHIFT_RIGHT => ">>",
	TokenType.LOGIC_RIGHT_SHIFT => ">>>",
	TokenType.INCREMENT => "++",
	TokenType.DECREMENT => "--",
	TokenType.PLUS_EQUALS => "+=",
	TokenType.MINUS_EQUALS => "-=",
	TokenType.STAR_EQUALS => "*=",
	TokenType.SLASH_EQUALS => "/=",
	TokenType.PERCENT_EQUALS => "%=",
	TokenType.CARET_EQUALS => "^=",
	TokenType.AND_EQUALS => "&=",
	TokenType.PIPE_EQUALS => "|=",
	TokenType.SHIFT_LEFT_EQUALS => "<<=",
	TokenType.SHIFT_RIGHT_EQUALS => ">>=",
	TokenType.LOGIC_RIGHT_SHIFT_EQUALS => ">>>=",
	TokenType.EQUALS_GREATER_THAN => "=>",
	TokenType.BREAK => "break",
	TokenType.CASE => "case",
	TokenType.CAST => "cast",
	TokenType.CATCH => "catch",
	TokenType.CLASS => "class",
	TokenType.CONTINUE => "continue",
	TokenType.DEFAULT => "default",
	TokenType.DO => "do",
	TokenType.DYNAMIC => "dynamic",
	TokenType.ELSE => "else",
	TokenType.ENUM => "enum",
	TokenType.EXTENDS => "extends",
	TokenType.EXTERN => "extern",
	TokenType.FALSE => "false",
	TokenType.FOR => "for",
	TokenType.FUNCTION => "function",
	TokenType.IF => "if",
	TokenType.IMPLEMENTS => "implements",
	TokenType.IMPORT => "import",
	TokenType.IN => "in",
	TokenType.INLINE => "inline",
	TokenType.INTERFACE => "interface",
	TokenType.NEVER => "never",
	TokenType.NEW => "new",
	TokenType.NULL => "null",
	TokenType.OVERRIDE => "override",
	TokenType.PACKAGE => "package",
	TokenType.PRIVATE => "private",
	TokenType.PUBLIC => "public",
	TokenType.RETURN => "return",
	TokenType.STATIC => "static",
	TokenType.SUPER => "super",
	TokenType.SWITCH => "switch",
	TokenType.THIS => "this",
	TokenType.THROW => "throw",
	TokenType.TRACE => "trace",
	TokenType.TRUE => "true",
	TokenType.TRY => "try",
	TokenType.TYPEDEF => "typedef",
	TokenType.UNTYPED => "untyped",
	TokenType.USING => "using",
	TokenType.VAR => "var",
	TokenType.WHILE => "while"
	];

	public inline function typeString():String
	{
		return tokens.get(type);
	}

	public function isKeyword():Bool
	{
		return type == TokenType.ABSTRACT || type == TokenType.BREAK ||
		type == TokenType.CASE || type == TokenType.CAST ||
		type == TokenType.CATCH || type == TokenType.CLASS ||
		type == TokenType.CONTINUE || type == TokenType.DEFAULT ||
		type == TokenType.DO || type == TokenType.DYNAMIC ||
		type == TokenType.ELSE || type == TokenType.ENUM ||
		type == TokenType.EXTENDS || type == TokenType.EXTERN ||
		type == TokenType.FALSE || type == TokenType.FOR ||
		type == TokenType.FUNCTION || type == TokenType.IF ||
		type == TokenType.IMPLEMENTS || type == TokenType.IMPORT ||
		type == TokenType.IN || type == TokenType.INLINE ||
		type == TokenType.INTERFACE || type == TokenType.MACRO ||
		type == TokenType.NEW || type == TokenType.NULL ||
		type == TokenType.OVERRIDE || type == TokenType.PACKAGE ||
		type == TokenType.PRIVATE || type == TokenType.PUBLIC ||
		type == TokenType.RETURN || type == TokenType.STATIC ||
		type == TokenType.SWITCH || type == TokenType.THIS ||
		type == TokenType.THROW || type == TokenType.TRUE ||
		type == TokenType.TRY || type == TokenType.TYPEDEF ||
		type == TokenType.UNTYPED || type == TokenType.USING ||
		type == TokenType.VAR || type == TokenType.WHILE;
	}

	public function isSingleSymbol():Bool
	{
		return type == TokenType.SEMICOLON || type == TokenType.COLON ||
		type == TokenType.COMMA || type == TokenType.LBRACE ||
		type == TokenType.RBRACE || type == TokenType.LPAREN ||
		type == TokenType.RPAREN || type == TokenType.LBRACKET ||
		type == TokenType.RBRACKET || type == TokenType.LESSTHAN ||
		type == TokenType.GREATERTHAN || type == TokenType.PLUS ||
		type == TokenType.MINUS || type == TokenType.STAR ||
		type == TokenType.EQUALS || type == TokenType.SLASH ||
		type == TokenType.DOT || type == TokenType.EXCLAMATION ||
		type == TokenType.QUESTION || type == TokenType.AND ||
		type == TokenType.PIPE || type == TokenType.PERCENT ||
		type == TokenType.CARET || type == TokenType.HASH ||
		type == TokenType.AT;
	}

	public function isCompoundSymbol():Bool
	{
		return type == TokenType.LOGIC_NOT_EQUALS || type == TokenType.LOGIC_EQUALS ||
		type == TokenType.LOGIC_GREATER_THAN_EQUALS || type == TokenType.LOGIC_LESS_THAN_EQUALS ||
		type == TokenType.LOGIC_AND || type == TokenType.LOGIC_OR ||
		type == TokenType.SHIFT_LEFT || type == TokenType.SHIFT_RIGHT ||
		type == TokenType.LOGIC_RIGHT_SHIFT || type == TokenType.INCREMENT ||
		type == TokenType.DECREMENT || type == TokenType.PLUS_EQUALS ||
		type == TokenType.MINUS_EQUALS || type == TokenType.STAR_EQUALS ||
		type == TokenType.SLASH_EQUALS || type == TokenType.PERCENT_EQUALS ||
		type == TokenType.CARET_EQUALS || type == TokenType.AND_EQUALS ||
		type == TokenType.PIPE_EQUALS || type == TokenType.SHIFT_LEFT_EQUALS ||
		type == TokenType.SHIFT_RIGHT_EQUALS || type == TokenType.LOGIC_RIGHT_SHIFT_EQUALS ||
		type == TokenType.EQUALS_GREATER_THAN;
	}

	public inline function isSymbol():Bool
	{
		return isSingleSymbol() || isCompoundSymbol();
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
		if (cur >= content.length) {
			return-1;
		}
		var c = StringTools.fastCodeAt(content, cur);
		return c;
	}

	private inline function next()
	{
		cur++;
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
		return c == " ".code || c == "\t".code || c == "\r".code;
	}

	private inline function isNumber(c:Int):Bool
	{
		return c >= "0".code && c <= "9".code;
	}

	private inline function isLowerAlpha(c:Int):Bool
	{
		return c >= "a".code && c <= "z".code;
	}

	private inline function isUpperAlpha(c:Int):Bool
	{
		return c >= "A".code && c <= "Z".code;
	}

	private inline function isAlpha(c:Int):Bool
	{
		return isLowerAlpha(c) || isUpperAlpha(c);
	}

	private inline function skipWhitespacePeek():Int
	{
		var c = peek();
		while (isWhitespace(c)) 		{
			next();
			c = peek();
		}
		return c;
	}

	// INFO: this is not correct. it detects number-like things only
	private inline function number(c:Int):Token
	{
		mark();
		var startsWithDot = c == ".".code;
		next();
		c = peek();
		if (startsWithDot && !isNumber(c)) 		{
			return new Token(TokenType.DOT);
		}
		while (isNumber(c) || c == ".".code || isAlpha(c)) 		{
			next();
			c = peek();
		}
		return new Token(TokenType.INT, marked());
	}

	private inline function string(c:Int):Token
	{
		mark();
		var end = c == "\"".code?"\"".code:"'".code;
		var ignoreNext = false;
		do {
			ignoreNext = !ignoreNext && c == "\\".code;
			next();
			c = peek();
		} while (c != -1 && !(!ignoreNext && c == end));
		next();
		return new Token(TokenType.STRING, marked());
	}

	private inline function slash():Token
	{
		mark();
		next();
		var c = peek();
		switch (c) 		{
			case "/".code:
			do {
				next();
				c = peek();
			} while (c != -1 && c != "\n".code);
			return new Token(TokenType.COMMENT, marked());
			case "*".code:
			var last:Int;
			do {
				next();
				last = c;
				c = peek();
			} while (c != -1 && !(last == "*".code && c == "/".code));
			return new Token(TokenType.COMMENT, marked());
			case "=".code:
			next();
			return new Token(TokenType.STAR_EQUALS);
			default:
			return new Token(TokenType.STAR);
		}
	}

	// INFO: this does not match the 0x7F to 0xFF range
	// [a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*
	private inline function identifier():Token
	{
		mark();
		next();
		var c = peek();
		while (c == "_".code || isAlpha(c) || isNumber(c)) 		{
			next();
			c = peek();
		}
		var str = marked();
		var type = switch (str) 		{
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
		};
		var tok = new Token(type);
		if (type == TokenType.IDENTIFIER)
		tok.value = str;
		return tok;
	}

	private inline function equals():Token
	{
		next();
		switch (peek()) 		{
			case "=".code:
			next();
			return new Token(TokenType.LOGIC_EQUALS);
			case ">".code:
			next();
			return new Token(TokenType.EQUALS_GREATER_THAN);
			default:
			return new Token(TokenType.EQUALS);
		}
	}

	private inline function exclamation():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.LOGIC_NOT_EQUALS);
		}
		return new Token(TokenType.EXCLAMATION);
	}

	private inline function greatherthan():Token
	{
		next();
		switch (peek()) 		{
			case "=".code:
			next();
			return new Token(TokenType.LOGIC_GREATER_THAN_EQUALS);
			case ">".code:
			next();
			switch (peek()) 			{
				case ">".code:
				next();
				if (peek() == "=".code) 				{
					return new Token(TokenType.LOGIC_RIGHT_SHIFT_EQUALS);
				}
				return new Token(TokenType.LOGIC_RIGHT_SHIFT);
				case "=".code:
				next();
				return new Token(TokenType.SHIFT_RIGHT_EQUALS);
				default:
				return new Token(TokenType.SHIFT_RIGHT);
			}
			default:
			return new Token(TokenType.GREATERTHAN);
		}
	}

	private inline function lessthan():Token
	{
		next();
		switch (peek()) 		{
			case "=".code:
			next();
			return new Token(TokenType.LOGIC_LESS_THAN_EQUALS);
			case "<".code:
			next();
			if (peek() == "=".code) 			{
				next();
				return new Token(TokenType.SHIFT_LEFT_EQUALS);
			}
			return new Token(TokenType.SHIFT_LEFT);
			default:
			return new Token(TokenType.LESSTHAN);
		}
	}

	private inline function plus():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.PLUS_EQUALS);
		}
		return new Token(TokenType.PLUS);
	}

	private inline function minus():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.MINUS_EQUALS);
		}
		return new Token(TokenType.MINUS);
	}

	private inline function star():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.STAR_EQUALS);
		}
		return new Token(TokenType.STAR);
	}

	private inline function percent():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.PERCENT_EQUALS);
		}
		return new Token(TokenType.PERCENT);
	}

	private inline function caret():Token
	{
		next();
		if (peek() == "=".code) 		{
			next();
			return new Token(TokenType.CARET_EQUALS);
		}
		return new Token(TokenType.CARET);
	}

	private inline function pipe():Token
	{
		next();
		switch (peek()) 		{
			case "=".code:
			next();
			return new Token(TokenType.PIPE_EQUALS);
			case "|".code:
			next();
			return new Token(TokenType.LOGIC_OR);
			default:
			return new Token(TokenType.PIPE);
		}
	}

	private inline function and():Token
	{
		next();
		switch (peek()) 		{
			case "=".code:
			next();
			return new Token(TokenType.AND_EQUALS);
			case "&".code:
			next();
			return new Token(TokenType.LOGIC_AND);
			default:
			return new Token(TokenType.AND);
		}
	}

	public function token():Token
	{
		var c = skipWhitespacePeek();
		switch (c) 		{
			case-1:return new Token(TokenType.EOF);
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
			case "<".code:return lessthan();
			case ">".code:return greatherthan();
			case "+".code:return plus();
			case "-".code:return minus();
			case "*".code:return star();
			case "=".code:return equals();
			case "!".code:return exclamation();
			case "?".code:next();return new Token(TokenType.QUESTION);
			case "&".code:return and();
			case "|".code:return pipe();
			case "%".code:return percent();
			case "^".code:return caret();
			case "#".code:next();return new Token(TokenType.HASH);
			case "@".code:next();return new Token(TokenType.AT);
			case "/".code:return slash();
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
			return identifier();
			case _:
			return new Token(TokenType.UNEXPECTED, String.fromCharCode(c));
		};
	}
}