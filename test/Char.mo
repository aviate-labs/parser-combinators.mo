import Text "mo:base/Text";

import C "../src/Combinators";
import L "../src/List";

let a = C.Char.char('a');
let input = L.fromIter(Text.toIter("abc"));

let (c, i) = a(input)[0];
assert(c == 'a');
assert("bc" == Text.fromIter(L.toIter(i)));

let r = a(i);
assert(r.size() == 0);
assert("bc" == Text.fromIter(L.toIter(i)));
