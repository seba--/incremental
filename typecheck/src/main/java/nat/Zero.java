package nat;

import bool.*;

public class Zero extends Nat {
    public Zero() {
        super();
    }

    @Override
    public Bool isZero() {
        return new True();
    }

    @Override
    public Nat pred() {
        return this;
    }

    @Override
    public Bool lessThan(Nat that) {
        return that.isNonZero();
    }

    @Override
    public Bool equal(Nat that) {
        return that.isZero();
    }

    @Override
    public Nat plus(Nat that) {
        return that;
    }
}
