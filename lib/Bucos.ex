# File: lib/Bucos.ex
# This file was generated from src/bucos.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucos do
	def unquote(:"run")(arg1) do
		:erlang.apply(:"bucos", :"run", [arg1])
	end
	def unquote(:"run")(arg1, arg2) do
		:erlang.apply(:"bucos", :"run", [arg1, arg2])
	end
	def unquote(:"in")(arg1, arg2, arg3) do
		:erlang.apply(:"bucos", :"in", [arg1, arg2, arg3])
	end
	def unquote(:"in")(arg1, arg2) do
		:erlang.apply(:"bucos", :"in", [arg1, arg2])
	end
end
