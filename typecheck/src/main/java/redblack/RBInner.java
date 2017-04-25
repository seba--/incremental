package redblack;

import bool.Bool;
import bool.False;
import nat.Nat;

public class RBInner extends RBNode {
    public Color color;
    public Nat data;
    public RBNode left;
    public RBNode right;
    public RBInner(Color color, Nat data, RBNode left, RBNode right) {
        super();
        this.color = color;
        this.data = data;
        this.left = left;
        this.right = right;
    }

    @Override
    public Nat data() {
        return this.data;
    }

    @Override
    public RBNode left() {
        return this.left;
    }

    @Override
    public RBNode right() {
        return this.right;
    }

    @Override
    public Color color() {
        return this.color;
    }

    @Override
    public Bool isNil() {
        return new False();
    }

    public RBNode withBlackColor() {
        return new RBInner(new Color(new False()), data(), left(), right());
    }
}
