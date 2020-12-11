
-define(GEN_NAME(Module, Name), {via, cache_ns, {Module, Name}}).
-define(GEN_NAME(Name), {via, cache_ns, {?MODULE, Name}}).
