import logging
import os
import pathlib
import shlex


def strIII(s):
	"""do not convert if S is a string or represents a path
	>>> strIII("a")
	Traceback (most recent call last):
	...
	AssertionError: 'a' is a <class 'str'> object
	>>> strIII(pathlib.PurePath()) # doctest: +ELLIPSIS
	Traceback (most recent call last):
	...
	AssertionError: ..."""
	import builtins
	assert not isinstance(s, (builtins.str, pathlib.PurePath)), "{} is a {} object".format(repr(s), str(type(s)))
	return builtins.str(s)


def os_fspath(p: pathlib.PurePath):
	"""convert to string if object represents a path"""
	# https://docs.python.org/dev/library/os.html#os.fspath
	assert isinstance(p, pathlib.PurePath)
	return str(p)


def raise_if_not(cond: bool, msg: str, logger: logging.Logger = None) -> None:
	assert type(cond) == bool
	if not cond:
		if logger is not None:
			logger.critical(msg)
		raise RuntimeError(msg)


def raise_if(cond: bool, msg: str, logger: logging.Logger = None) -> None:
	assert type(cond) == bool
	if cond:
		if logger is not None:
			logger.critical(msg)
		raise RuntimeError(msg)


def str_to_path(s: str) -> pathlib.Path:
	"expand vars, user and return a path object"
	p = pathlib.Path(os.path.expandvars(s)).expanduser()
	# if not p.is_absolute():
	# raise ValueError("path is not absolute: {}".format(p))
	return p.absolute()


def unexpanduser_quote(s: str) -> str:
	def unexpanduser(s: str) -> str:
		HOME = os.path.expanduser("~")
		if s.startswith(HOME):
			return "~" + s[len(HOME):]
		return s

	return unexpanduser(shlex.quote(s))


def list_as_shell_cmd(cmd: list) -> str:
	"list to shell command"
	return " ".join(map(unexpanduser_quote, cmd))



def name_no_ext(p: pathlib.PurePath) -> str:
	"name without any extensions"
	return p.name[:len(p.name) - sum(map(len, p.suffixes))]


if __name__ == "__main__":
	import doctest

	doctest.testmod(verbose=True)
