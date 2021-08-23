import C "../src/Combinators";
import L "../src/List";

let int = C.Int.int();
switch (int(L.fromText("-100"))) {
    case (null) { assert(false); };
    case (? (x, xs)) {
        assert(x == -100);
        assert(xs == null);
    };
};
