import Array "mo:base/Array";
import Iter "mo:base/Iter";
import List "mo:base/List";

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

    // If the parser succeeds, the value of r applied with the given function.
    public func map<T, R, S>(
        parser : Parser<T, R>,
        function : R -> S,
    ) : Parser<T, S> {
        bind<T, R, S>(
            parser,
            func (r : R) {
                Parser.result(function(r));
            },
        );
    };

    // Ignore-left combinator.
    // Parses r and then s. Returns the value returned by s.
    public func right<T, R, S>(
        parserR : Parser<T, R>,
        parserS : Parser<T, S>
    ) : Parser<T, S> {
        bind<T, R, S>(
            parserR,
            func (_ : R) {
                parserS;
            },
        );
    };

    // Ignore-right combinator.
    // Parses r and then s. Returns the value returned by r.
    public func left<T, R, S>(
        parserR : Parser<T, R>,
        parserS : Parser<T, S>
    ) : Parser<T, R> {
        bind<T, R, R>(
            parserR,
            func (r : R) {
                bind<T, S, R>(
                    parserS,
                    func (_ : S) {
                        Parser.result(r);
                    },
                );
            },
        );
    };

    public func default<T, R>(
        parser : Parser<T, R>,
        default : R,
    ) : Parser<T, R> {
        choose(parser, Parser.result<T, R>(default));
    };

    public func ignoreMany<T, R>(
        parser : Parser<T, R>,
    ) : Parser<T, ()> {
        default(
            bind<T, R, ()>(
                parser,
                func (_ : R) {
                    ignoreMany(parser);
                },
            ),
            (),
        );
    };

    public func ignoreMany1<T, R>(
        parser : Parser<T, R>,
    ) : Parser<T, ()> {
        right(parser, ignoreMany(parser));
    };

    public func oneOf<T>(
        xs : [T],
        equal : (T, T) -> Bool,
    ) : Parser<T, T> {
        satisfy(func(t : T) : Bool {
            for (x in xs.vals()) {
                if (equal(x, t)) {
                    return true;
                };
            };
            false;
        });
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

        public func space() : CharParser {
            oneOf<Char>(
                [' ', '\t', '\r', '\n'],
                func (x, y) { x == y },
            );
        };

        public func ignoreSpaces<R>(
            parser : Parser<Char, R>,
        ) : Parser<Char, R> {
            right(
                ignoreMany<Char, Char>(space()),
                parser,
            );
        };
    };

    public module Text {
        public func text(t : Text) : Parser<Char, Text> {
            func iter(i : Iter.Iter<Char>) : Parser<Char, Text> {
                switch (i.next()) {
                    case (null) { Parser.result(t); };
                    case (? v)  {
                        right(
                            Char.char(v),
                            iter(i),
                        );
                    };
                };
            };
            Char.ignoreSpaces(iter(t.chars()));
        };
    };
};
