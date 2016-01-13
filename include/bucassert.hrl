-include_lib("stdlib/include/assert.hrl").
-define(assertContinueIfMatch(Guard, Expr, Param, With),
        begin
          ((fun () ->
                case (Expr) of
                  Guard -> With(Param);
                  Value -> erlang:error({assertContinueIfMatch,
                                         [{module, ?MODULE},
                                          {line, ?LINE},
                                          {expression, (??Expr)},
                                          {pattern, (??Guard)},
                                          {value, Value}]})
                end
            end)())
        end).

