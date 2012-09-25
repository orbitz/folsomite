-module(folsomite_utils).

-export([ any2l/1
        , space2dot/1
        ]).


any2l(X) when is_list(X)    -> X;
any2l(X) when is_atom(X)    -> atom_to_list(X);
any2l(X) when is_integer(X) -> integer_to_list(X);
any2l(X) when is_float(X)   -> float_to_list(X);
any2l(X) when is_tuple(X)   -> string:join([any2l(A) || A <- tuple_to_list(X)], " ").


space2dot(X) -> string:join(string:tokens(X, " "), ".").
