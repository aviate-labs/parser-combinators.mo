import List "mo:base/List";

module {
    public func fromIter<T>(i : Iter.Iter<T>) : List<T> {
        switch (i.next()) {
            case (null) { null; };
            case (? v)  { ?(v, fromIter(i)); };
        };
    };

    public class toIter<T>(xs : List<T>) : Iter.Iter<T> {
        var list = xs;
        public func next() : ?T {
            switch (list) {
                case (null) { null; };
                case (? ys) {
                    let (x, xs) = ys;
                    list := xs;
                    ?x;
                };
            };
        };
    };
}