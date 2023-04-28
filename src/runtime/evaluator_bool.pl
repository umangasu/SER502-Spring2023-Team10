boolean(t_boolean(true)) --> [true].
boolean(t_boolean(false)) --> [false].

condition(t_cond_bool(Boolean)) --> boolean(Boolean).
condition(t_cond_negate(Condition)) --> [!], condition(Condition).