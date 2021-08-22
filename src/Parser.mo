import Iter "mo:base/Iter";
import Result "mo:base/Result";

module {
    public type Parser<T, R> = Iter.Iter<T> -> [(R, Iter.Iter<T>)];

    // Succeeds without consuming any of the input, and returns the single result x.
    public func result<T, R>(x : R) : Parser<T, R> {
        func (i : Iter.Iter<T>) { [(x, i)]; };
    };

    // Always fails, regardless of the input.
    public func zero<T, R>() : Parser<T, R> {
        func (_ : Iter.Iter<T>) { []; };
    };

    // Successfully consumes the first item if the input is non-empty, and fails otherwise.
    public func item<T>() : Parser<T, T> {
        func (i : Iter.Iter<T>) {
            switch (i.next()) {
                case (null) { [];       };
                case (? v)  { [(v, i)]; };
            };
        };
    };
};
