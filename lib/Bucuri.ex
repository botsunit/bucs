# File: lib/Bucuri.ex
# This file was generated from src/bucuri.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucuri do
	def unquote(:"join")(arg1, arg2) do
		:erlang.apply(:"bucuri", :"join", [arg1, arg2])
	end
	def unquote(:"join")(arg1) do
		:erlang.apply(:"bucuri", :"join", [arg1])
	end
end
