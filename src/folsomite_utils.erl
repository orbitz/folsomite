-module(folsomite_utils).

-export([ scalar2str/1
        , tuple2list/1
        , space2dot/1
        ]).


scalar2str(X) when is_list(X)    -> X;
scalar2str(X) when is_atom(X)    -> atom_to_list(X);
scalar2str(X) when is_integer(X) -> integer_to_list(X);
scalar2str(X) when is_float(X)   -> float_to_list(X).

tuple2list(X) when is_tuple(X) -> lists:map(fun tuple2list/1, tuple_to_list(X));
tuple2list(X) when is_list(X)  -> lists:map(fun tuple2list/1, X);
tuple2list(X)                  -> X.

space2dot(X) -> string:join(string:tokens(X, " "), ".").
