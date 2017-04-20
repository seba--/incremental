package nat;

import bool.*;

public class Nat {
    public Nat() {}
    public /*abstract*/ Bool isZero() { return new False(); }
    public Bool isNonZero() {
        return isZero().not();
    }
    public /*abstract*/ Nat pred() { return new Zero(); }
    public /*abstract*/ Bool lessThan(Nat that) { return new False(); }
    public /*abstract*/ Bool equal(Nat that) { return new False(); }
    public Bool greaterThan(Nat that) {
        return that.lessThan(this);
    }
    public /*abstract*/ Nat plus(Nat that) { return new Zero(); }
}
