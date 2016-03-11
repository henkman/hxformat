package hxformat;

import hxformat.Tokenizer.Token;
import hxformat.Tokenizer.TokenType;
import sys.FileSystem;
import sys.io.File;

class Formatter
{
	var tokenizer:Tokenizer;
	var buffer:StringBuf;
	var cur:Token;
	var next:Token;
	var block:Int;
	var newline:Bool;
	var infunction:Int;

	private inline function new(content:String)
	{
		tokenizer = new Tokenizer(content);
		buffer = new StringBuf();
		block = 0;
		newline = false;
		infunction = 0;
	}

	private function node()
	{
		if (next.type == TokenType.LBRACE && cur.type == TokenType.NEWLINE) {
			if (!isInFunction()) {
				buffer.add("\n");
			} else {
				buffer.add(" ");
			}
			return;
		}

		if (newline && cur.type != TokenType.NEWLINE) {
			var blocks = block;
			if (cur.type == TokenType.RBRACE) {
				blocks--;
			}
			for (i in 0...blocks) {
				buffer.add("\t");
			}
		}

		if (cur.type == TokenType.LBRACE) {
			block++;
		} else if (cur.type == TokenType.RBRACE) {
			block--;
		}

		buffer.add(cur);

		if ((cur.type == TokenType.IF ||
		cur.type == TokenType.FOR ||
		cur.type == TokenType.DO ||
		cur.type == TokenType.SWITCH ||
		cur.type == TokenType.WHILE ||
		cur.type == TokenType.COMMA
		) && next.type != TokenType.NEWLINE) {
			buffer.add(" ");
		}
		else if (cur.type == TokenType.EQUALS &&
		next.type != TokenType.NEWLINE &&
		next.type != TokenType.EQUALS &&
		next.type != TokenType.GREATERTHAN) {
			buffer.add(" ");
		}
		else if (
		(
		cur.type == TokenType.IDENTIFIER ||
		cur.type == TokenType.INT ||
		cur.isKeyword()
		) && (
		next.type == TokenType.IDENTIFIER ||
		next.type == TokenType.INT ||
		next.isKeyword() ||
		next.type == TokenType.EQUALS ||
		next.type == TokenType.STRING
		)
		) {
			buffer.add(" ");
		}
		else if (cur.type == TokenType.RBRACE && (next.type == TokenType.ELSE || next.type == TokenType.WHILE)) {
			buffer.add(" ");
		}
		else if ((cur.isCompoundSymbol() || next.isCompoundSymbol()) && next.type != TokenType.NEWLINE) {
			buffer.add(" ");
		}
		else if (next.type == TokenType.LBRACE) {
			if (!isInFunction()) {
				buffer.add("\n");
			} else {
				buffer.add(" ");
			}
		}
	}

	private inline function isInFunction():Bool
	{
		return infunction != 0;
	}

	private function get()
	{
		newline = cur.type == TokenType.NEWLINE;
		cur = next;
		next = tokenizer.token();
	}

	private function funcbody()
	{
		for (i in 0...block) {
			buffer.add("\t");
		}
		buffer.add("{");
		var startblock = block;
		infunction++;
		block++;
		while (block>startblock) {
			get();
			node();
		}
		infunction--;
	}

	private inline function func()
	{
		node();
		get();
		var name = cur.value;
		while (cur.type != TokenType.RPAREN) {
			node();
			get();
		}
		node();
		get();// )
		if (cur.type == TokenType.LBRACE) {
			funcbody();
		} else if (cur.type == TokenType.NEWLINE) {
			while (cur.type == TokenType.NEWLINE) {
				get();
			}
			if (cur.type == TokenType.LBRACE) {
				buffer.add("\n");
				funcbody();
			} else {
				node();
			}
		} else if (cur.type == TokenType.COLON) {
			while (cur.type != TokenType.LBRACE) {
				if (cur.type != TokenType.NEWLINE) {
					buffer.add(cur);
				}
				get();
			}
			buffer.add("\n");
			funcbody();
		} else {
			node();

		}
	}

	private function format():String
	{
		cur = tokenizer.token();
		next = tokenizer.token();
		while (cur.type != TokenType.EOF) 		{
			if (cur.type == TokenType.UNEXPECTED) 			{
				throw "unexpected token "+cur.value;
			}
			if (cur.type == TokenType.FUNCTION) 			{
				func();
			} else 			{
				node();
			}
			get();
		}
		return buffer.toString();
	}

	public static function formatFile(file:String, backup:Bool)
	{
		var content = File.getContent(file);
		var formatted = formatString(content);
		if (backup) {
			var backupFile = file+".bak";
			File.saveContent(backupFile, content);
		}
		#if debug
		Sys.println(formatted);
		#else
		File.saveContent(file, formatted);
		#end
	}

	public static function formatString(content:String):String
	{
		return new Formatter(content).format();
	}
}