import C "../src/Combinators";
import L "../src/List";
// import Debug "mo:base-0.7.3/Debug";

let nat = C.Float.fraction();
switch (nat(L.fromText("10.1"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 10.1);
        assert(xs == null);
    };
};
switch (nat(L.fromText("10"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 10);
        assert(xs == null);
    };
};
switch (nat(L.fromText("0.13"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == 0.13);
        assert(xs == null);
    };
};
switch (nat(L.fromText("-0.13"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == -0.13);
        assert(xs == null);
    };
};
