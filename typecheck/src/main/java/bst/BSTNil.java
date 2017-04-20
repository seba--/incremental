package bst;

import bool.Bool;
import bool.True;
import nat.Nat;
import nat.Zero;

public class BSTNil extends BSTNode {
    public BSTNil() {
        super();
    }

    @Override
    public Nat data() {
        return new Zero();
    }

    @Override
    public BSTNode left() {
        return this;
    }

    @Override
    public BSTNode right() {
        return this;
    }

    @Override
    public Bool isNil() {
        return new True();
    }
}
