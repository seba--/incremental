package nat;

import bool.*;

public class Succ extends Nat {
    public Nat pred;

    public Succ(Nat pred) {
        super();
        this.pred = pred;
    }

    @Override
    public Bool isZero() {
        return new False();
    }

    @Override
    public Nat pred() {
        return this.pred;
    }

    @Override
    public Bool lessThan(Nat that) {
        return that.isNonZero().and(this.pred.lessThan(that.pred()));
    }

    @Override
    public Bool equal(Nat that) {
        return that.isNonZero().and(this.pred.equal(that.pred()));
    }

    @Override
    public Nat plus(Nat that) {
        return new Succ(this.pred.plus(that));
    }
}
