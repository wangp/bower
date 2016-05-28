%-----------------------------------------------------------------------------%

:- module splitmix64.
:- interface.

:- type splitmix64.

    % The state can be seeded with any value.
    %
:- pred init(int::in, splitmix64::out) is det.

:- pred next(int::out, splitmix64::in, splitmix64::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Adapted from <http://xorshift.di.unimi.it/splitmix64.c>

/*  Written in 2015 by Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

/* This is a fixed-increment version of Java 8's SplittableRandom generator
   See http://dx.doi.org/10.1145/2714064.2660195 and
   http://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html

   It is a very fast generator passing BigCrush, and it can be useful if
   for some reason you absolutely want 64 bits of state; otherwise, we
   rather suggest to use a xoroshiro128+ (for moderately parallel
   computations) or xorshift1024* (for massively parallel computations)
   generator. */

:- pragma foreign_decl("C", "#include <stdint.h>").

:- pragma foreign_type("C", splitmix64, "uint64_t").

:- pragma foreign_proc("C",
    init(Seed::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = Seed;
").

:- pragma foreign_proc("C",
    next(Next::out, X0::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint64_t z;
    X = X0 + UINT64_C(0x9E3779B97F4A7C15);
    z = X;
    z = (z ^ (z >> 30)) * UINT64_C(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)) * UINT64_C(0x94D049BB133111EB);
    z = (z ^ (z >> 31));
    Next = z;
").

%-----------------------------------------------------------------------------%
