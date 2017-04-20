package nat;

import bool.*;

public abstract class Nat {
    public Nat() {}
    public abstract Bool isZero();
    public Bool isNonZero() {
        return isZero().not();
    }
    public abstract Nat pred();
    public abstract Bool lessThan(Nat that);
    public abstract Bool equal(Nat that);
    public Bool greaterThan(Nat that) {
        return that.lessThan(this);
    }
    public abstract Nat plus(Nat that);
}
