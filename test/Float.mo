import C "../src/Combinators";
import L "../src/List";
import F "mo:base-0.7.3/Float";
import Debug "mo:base-0.7.3/Debug";

let fraction = C.Float.fraction();
switch (fraction(L.fromText("10.1"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 10.1);
        assert(xs == null);
    };
};
switch (fraction(L.fromText("10"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 10);
        assert(xs == null);
    };
};
switch (fraction(L.fromText("0.13"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 0.13);
        assert(xs == null);
    };
};
switch (fraction(L.fromText("-0.13"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == -0.13);
        assert(xs == null);
    };
};

let float = C.Float.float();
switch (float(L.fromText("-0.13e-1"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(F.abs(x - -0.013) < 10e-8);
        assert(xs == null);
    };
};
