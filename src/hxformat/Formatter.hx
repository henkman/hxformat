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

	private inline function new(content:String)
	{
		tokenizer = new Tokenizer(content);
		buffer = new StringBuf();
		block = 0;
		newline = false;
	}

	private function format():String
	{
		cur = tokenizer.token();
		next = tokenizer.token();
		while (cur.type != TokenType.EOF) {
			if (cur.type == TokenType.UNEXPECTED) {
				throw "unexpected token "+cur.value;
			}

			if (newline&&cur.type != TokenType.NEWLINE) {
				var blocks = block;
				if (cur.type == TokenType.RBRACE) {
					blocks --;
				}
				for (i in 0...blocks) {
					buffer.add("\t");
				}
			}

			if (cur.type == TokenType.LBRACE) {
				block ++;
			} else if (cur.type == TokenType.RBRACE) {
				block --;
			}

			buffer.add(cur);

			if ((cur.type == TokenType.IF||
			cur.type == TokenType.FOR||
			cur.type == TokenType.DO||
			cur.type == TokenType.SWITCH||
			cur.type == TokenType.WHILE||
			cur.type == TokenType.COMMA
			)&&next.type != TokenType.NEWLINE) {
				buffer.add(" ");
			}
			else if (cur.type == TokenType.EQUALS&&
			next.type != TokenType.NEWLINE&&
			next.type != TokenType.EQUALS&&
			next.type != TokenType.GREATERTHAN) {
				buffer.add(" ");
			}
			else if (cur.type == TokenType.LBRACE&&
			next.type != TokenType.NEWLINE) {
				buffer.add(" ");
			}
			else if (cur.type == TokenType.RBRACE&&
			next.type != TokenType.NEWLINE) {
				buffer.add(" ");
			}
			else if (
			(
			cur.type == TokenType.IDENTIFIER||
			cur.type == TokenType.INT||
			cur.isKeyword()
			)&&(
			next.type == TokenType.IDENTIFIER||
			next.type == TokenType.INT||
			next.isKeyword()||
			next.type == TokenType.EXCLAMATION||
			next.type == TokenType.QUESTION||
			next.type == TokenType.PLUS||
			next.type == TokenType.STAR||
			next.type == TokenType.SLASH||
			next.type == TokenType.MINUS||
			next.type == TokenType.EQUALS||
			next.type == TokenType.STRING||
			next.type == TokenType.LBRACKET
			)
			) {
				buffer.add(" ");
			}
			else if (cur.type != TokenType.NEWLINE&&
			next.type == TokenType.LBRACE) {
				buffer.add(" ");
			}

			newline = cur.type == TokenType.NEWLINE;
			cur = next;
			next = tokenizer.token();
		}
		return buffer.toString();
	}

	public static function formatFile(file:String, backup:Bool)
	{
		var content = File.getContent(file);
		var formatted = formatString(content);
		if (backup) {
			var backupFile = file +".bak";
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
