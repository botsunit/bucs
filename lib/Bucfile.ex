# File: lib/Bucfile.ex
# This file was generated from src/bucfile.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucfile do
	def unquote(:"expand_path")(arg1) do
		:erlang.apply(:"bucfile", :"expand_path", [arg1])
	end
	def unquote(:"normalize_path")(arg1) do
		:erlang.apply(:"bucfile", :"normalize_path", [arg1])
	end
	def unquote(:"user_home")() do
		:erlang.apply(:"bucfile", :"user_home", [])
	end
	def unquote(:"make_dir")(arg1) do
		:erlang.apply(:"bucfile", :"make_dir", [arg1])
	end
	def unquote(:"remove_recursive")(arg1) do
		:erlang.apply(:"bucfile", :"remove_recursive", [arg1])
	end
	def unquote(:"copy")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"copy", [arg1, arg2])
	end
	def unquote(:"copy")(arg1, arg2, arg3) do
		:erlang.apply(:"bucfile", :"copy", [arg1, arg2, arg3])
	end
	def unquote(:"copyfile")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"copyfile", [arg1, arg2])
	end
	def unquote(:"copyfile")(arg1, arg2, arg3) do
		:erlang.apply(:"bucfile", :"copyfile", [arg1, arg2, arg3])
	end
	def unquote(:"is_executable")(arg1) do
		:erlang.apply(:"bucfile", :"is_executable", [arg1])
	end
	def unquote(:"is_executable")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"is_executable", [arg1, arg2])
	end
	def unquote(:"relative_from")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"relative_from", [arg1, arg2])
	end
	def unquote(:"realpath")(arg1) do
		:erlang.apply(:"bucfile", :"realpath", [arg1])
	end
	def unquote(:"wildcard")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"wildcard", [arg1, arg2])
	end
	def unquote(:"wildcard")(arg1, arg2, arg3) do
		:erlang.apply(:"bucfile", :"wildcard", [arg1, arg2, arg3])
	end
	def unquote(:"match")(arg1, arg2) do
		:erlang.apply(:"bucfile", :"match", [arg1, arg2])
	end
	def unquote(:"match")(arg1, arg2, arg3) do
		:erlang.apply(:"bucfile", :"match", [arg1, arg2, arg3])
	end
end
