import Char "mo:base/Char";
import List "mo:base/List";
import Nat32 "mo:base/Nat32";

import Parser "Parser";

import D "mo:base/Debug";

module {
    private type List<T> = List.List<T>;
    private type Parser<T, A> = Parser.Parser<T, A>;

    // Integrates the sequencing of parsers with the processing of their result values.
    public func bind<T, A, B>(
        parserA : Parser<T, A>,
        function : A -> Parser<T, B>,
    ) : Parser<T, B> {
        func (xs : List<T>) {
            switch (parserA(xs)) {
                case (null) { null };
                case (? (a, xs)) {
                    function(a)(xs);
                };
            };
        };
    };

    // Applies one parser after another, with the results from the two parsers being combined as pairs. 
    public func seq<T, A, B>(
        parserA : Parser<T, A>,
        parserB : Parser<T, B>,
    ) : Parser<T, (A, B)> {
        bind(
            parserA,
            func (a : A) : Parser<T, (A, B)> {
                bind(
                    parserB,
                    func (b : B) : Parser<T, (A, B)> { 
                        Parser.result((a, b));
                    },
                );
            },
        );
    };

    // Yields a parser that consumes a single item if it satisfies the predicate, and fails otherwise.
    public func sat<T>(
        ok : T -> Bool,
    ) : Parser<T, T> {
        bind(
            Parser.item<T>(),
            func (t : T) : Parser<T, T> {
                if (ok(t)) {
                    return Parser.result(t);
                };
                Parser.zero();
            },
        );
    };

    // Decides between parsing a single item and recursing, or parsing nothing further and terminating.
    public func choose<T, A>(
        parser1 : Parser<T, A>,
        parser2 : Parser<T, A>,
    ) : Parser<T, A> {
        func (xs : List<T>) {
            switch (parser1(xs)) {
                case (? x) { ?x; };
                case (null) {
                    parser2(xs);
                };
            };
        };
    };

    public func map<T, A, B>(
        parserA : Parser<T, A>,
        function : A -> B,
    ) : Parser<T, B>{
        bind(
            parserA,
            func (a : A) : Parser<T, B> {
                Parser.result<T, B>(function(a));
            },
        );
    };

    // Applies a parser p zero or more times to the input.
    public func many<T, A>(
        parserA : Parser<T, A>,
    ) : Parser<T, List<A>> {
        func (xs : List<T>) {
            switch (parserA(xs)) {
                case (null) { ?(null, xs) };
                case (? (a, xs)) {
                    switch (many(parserA)(xs)) {
                        case (null) { 
                            ?(?(a, List.nil<A>()), xs);
                        };
                        case (? (b, xs)) {
                            ?(?(a, b), xs);
                        };
                    }
                };
            };
        };
    };

    // Non-empty sequences of items.
    public func many1<T, A>(
        parserA : Parser<T, A>,
    ) : Parser<T, List<A>> {
        func (xs : List<T>) {
            switch (parserA(xs)) {
                case (null) { null; };
                case (? (a, xs)) {
                    switch (many(parserA)(xs)) {
                        case (null) { 
                            ?(?(a, List.nil<A>()), List.nil<T>());
                        };
                        case (? (b, xs)) {
                            ?(?(a, b), xs);
                        };
                    }
                };
            };
        };
    };

    public module Character {
        private type CharParser = Parser<Char, Char>;

        public func char(x : Char) : CharParser {
            sat(func (y : Char) : Bool { x == y });
        };

        public func digit() : CharParser {
            sat(func (x : Char) : Bool {
                '0' <= x and x <= '9';
            });
        };

        public func lower() : CharParser {
            sat(func (x : Char) : Bool {
                'a' <= x and x <= 'z';
            });
        };

        public func upper() : CharParser {
            sat(func (x : Char) : Bool {
                'A' <= x and x <= 'Z';
            });
        };

        public func letter() : CharParser {
            choose(lower(), upper());
        };

        public func alphanum() : CharParser {
            choose(letter(), digit());
        };
    };

    public module Text {
        private type StringParser = Parser<Char, List<Char>>;

        public func word() : StringParser {
            many(Character.letter());
        };
    };

    public module Nat {
        func toNat(xs : List<Char>) : Nat {
            let ord0 = Char.toNat32('0');
            let n = List.foldLeft<Char, Nat>(
                xs,
                0,
                func (n : Nat, c : Char) : Nat {
                    10 * n + Nat32.toNat((Char.toNat32(c) - ord0));
                },
            );
            n;
        };

        public func nat() : Parser<Char, Nat> {
            map(
                many1(Character.digit()),
                toNat,
            );
        };
    };

    public module Int {
        public func int() : Parser<Char, Int> {
            func (xs : List<Char>) {
                let (op, ys) = switch(Character.char('-')(xs)) {
                    case (null)      { (func (n : Nat) : Int {  n; }, xs); };
                    case (? (_, xs)) { (func (n : Nat) : Int { -n; }, xs); };
                };
                map<Char, Nat, Int>(
                    Nat.nat(),
                    op,
                )(ys);
            }
        };
    };
};
