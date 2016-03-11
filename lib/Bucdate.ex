# File: lib/Bucdate.ex
# This file was generated from src/bucdate.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucdate do
	def unquote(:"local_timezone")() do
		:erlang.apply(:"bucdate", :"local_timezone", [])
	end
	def unquote(:"add")(arg1, arg2, arg3) do
		:erlang.apply(:"bucdate", :"add", [arg1, arg2, arg3])
	end
	def unquote(:"add")(arg1, arg2) do
		:erlang.apply(:"bucdate", :"add", [arg1, arg2])
	end
	def unquote(:"today")() do
		:erlang.apply(:"bucdate", :"today", [])
	end
	def unquote(:"tomorrow")() do
		:erlang.apply(:"bucdate", :"tomorrow", [])
	end
	def unquote(:"yesterday")() do
		:erlang.apply(:"bucdate", :"yesterday", [])
	end
	def unquote(:"compare")(arg1, arg2) do
		:erlang.apply(:"bucdate", :"compare", [arg1, arg2])
	end
	def unquote(:"to_iso8601")(arg1) do
		:erlang.apply(:"bucdate", :"to_iso8601", [arg1])
	end
	def unquote(:"format")(arg1) do
		:erlang.apply(:"bucdate", :"format", [arg1])
	end
	def unquote(:"format")(arg1, arg2) do
		:erlang.apply(:"bucdate", :"format", [arg1, arg2])
	end
	def unquote(:"parse")(arg1) do
		:erlang.apply(:"bucdate", :"parse", [arg1])
	end
	def unquote(:"parse")(arg1, arg2) do
		:erlang.apply(:"bucdate", :"parse", [arg1, arg2])
	end
	def unquote(:"nparse")(arg1) do
		:erlang.apply(:"bucdate", :"nparse", [arg1])
	end
end
