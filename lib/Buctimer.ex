# File: lib/Buctimer.ex
# This file was generated from src/buctimer.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Buctimer do
	def unquote(:"verify")(arg1) do
		:erlang.apply(:"buctimer", :"verify", [arg1])
	end
	def unquote(:"next")(arg1) do
		:erlang.apply(:"buctimer", :"next", [arg1])
	end
	def unquote(:"next")(arg1, arg2) do
		:erlang.apply(:"buctimer", :"next", [arg1, arg2])
	end
end
