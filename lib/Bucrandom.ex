# File: lib/Bucrandom.ex
# This file was generated from src/bucrandom.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucrandom do
	def unquote(:"start_link")() do
		:erlang.apply(:"bucrandom", :"start_link", [])
	end
	def unquote(:"randstr")(arg1) do
		:erlang.apply(:"bucrandom", :"randstr", [arg1])
	end
	def unquote(:"init")(arg1) do
		:erlang.apply(:"bucrandom", :"init", [arg1])
	end
	def unquote(:"handle_call")(arg1, arg2, arg3) do
		:erlang.apply(:"bucrandom", :"handle_call", [arg1, arg2, arg3])
	end
	def unquote(:"handle_cast")(arg1, arg2) do
		:erlang.apply(:"bucrandom", :"handle_cast", [arg1, arg2])
	end
	def unquote(:"handle_info")(arg1, arg2) do
		:erlang.apply(:"bucrandom", :"handle_info", [arg1, arg2])
	end
	def unquote(:"terminate")(arg1, arg2) do
		:erlang.apply(:"bucrandom", :"terminate", [arg1, arg2])
	end
	def unquote(:"code_change")(arg1, arg2, arg3) do
		:erlang.apply(:"bucrandom", :"code_change", [arg1, arg2, arg3])
	end
end
