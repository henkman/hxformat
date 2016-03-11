package;

import haxe.io.Path;
import hxformat.Formatter;
import sys.FileSystem;

class Main
{
	static function formatDir(target:String, backup:Bool)
	{
		for (file in FileSystem.readDirectory(target)) {
			var sub = Path.join([target, file]);
			if (FileSystem.isDirectory(sub)) {
				formatDir(sub, backup);
			} else {
				Formatter.formatFile(sub, backup);
			}
		}
	}

	static function main()
	{
		var args = Sys.args();
		if (args.length == 0) {
			Sys.println("usage: hxformat [file|dir] (-r Recursive) (-b Backup)");
			return;
		}
		var target = args[0];
		var recursive = false;
		var backup = false;
		for (i in 1...args.length) {
			var option = args[i];
			if (option == "-r") {
				recursive = true;
			} else if (option == "-b") {
				backup = true;
			}
		}
		if (FileSystem.isDirectory(target)) {
			if (recursive) {
				formatDir(target, backup);
			} else {
				for (file in FileSystem.readDirectory(target)) {
					var sub = Path.join([target, file]);
					if (!FileSystem.isDirectory(sub)) {
						Formatter.formatFile(sub, backup);
					}
				}
			}
		} else {
			Formatter.formatFile(target, backup);
		}
	}
}