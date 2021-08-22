import Array "mo:base/Array";

import List "List";
import Parser "Parser";

module Combinators {
    private type List<T> = List.List<T>;
    private type Parser<T, R> = Parser.Parser<T, R>;

    // Bind applies the parser to the input, yielding a list of (value,input) 
    // pairs. Since function takes a value and returns a parser, it can be 
    // applied to each value (and unconsumed input) in turn. This results in a 
    // list of lists of (value, input) pairs, that is flattened to a single list.
    public func bind<T, R, S>(
        parser : Parser<T, R>, 
        function : R -> Parser<T, S>,
    ) : Parser<T, S> {
        func (xs : List<T>) {
            var s : [(S, List<T>)] = [];
            for ((r, xs) in parser(xs).vals()) {
                s := Array.append(s, function(r)(xs));
            };
            s;
        };
    };

    // Yields a parser that consumes a single character if it satisfies the 
    // predicate, and fails otherwise.
    public func satisfy<T>(ok : T -> Bool) : Parser<T, T> {
        bind(
            Parser.item<T>(), // Consumes a single character unconditionally.
            func(v : T) : Parser<T, T> {
                if (ok(v)) { return Parser.result(v); };
                Parser.zero();
            },
        );
    };

    // One ... or another.
    public func choose<T, R>(
        parser1 : Parser<T, R>,
        parser2 : Parser<T, R>
    ) : Parser<T, R> {
        func (xs : List<T>) {
            let r = parser1(xs);
            if (r.size() != 0) { return r; };
            parser2(xs);
        };
    };

    public module Char {
        type CharParser = Parser<Char, Char>;

        public func char(x : Char) : CharParser {
            satisfy(func(y : Char) : Bool {
                x == y;
            });
        };

        public func digit() : CharParser {
            satisfy(func(x : Char) : Bool {
                '0' <= x and x <= '9';
            });
        };

        public func lower() : CharParser {
            satisfy(func(x : Char) : Bool {
                'a' <= x and x <= 'z';
            });
        };

        public func upper() : CharParser {
            satisfy(func(x : Char) : Bool {
                'A' <= x and x <= 'Z';
            });
        };
    };
}