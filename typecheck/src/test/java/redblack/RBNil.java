package redblack;

import bool.Bool;
import bool.False;
import bool.True;
import nat.Nat;
import nat.Zero;

public class RBNil extends RBNode {
    public RBNil() {
        super();
    }

    @Override
    public Nat data() {
        return new Zero();
    }

    @Override
    public RBNode left() {
        return this;
    }

    @Override
    public RBNode right() {
        return this;
    }

    @Override
    public Color color() {
        return new Color(new False());
    }

    @Override
    public Bool isNil() {
        return new True();
    }
}
