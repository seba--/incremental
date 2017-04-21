package bst;

import bool.Bool;
import bool.False;
import nat.Nat;

public class BSTInner extends BSTNode {
    public Nat data;
    public BSTNode left;
    public BSTNode right;
    public BSTInner(Nat data, BSTNode left, BSTNode right) {
        super();
        this.data = data;
        this.left = left;
        this.right = right;
    }

    @Override
    public Nat data() {
        return this.data;
    }

    @Override
    public BSTNode left() {
        return this.left;
    }

    @Override
    public BSTNode right() {
        return this.right;
    }

    @Override
    public Bool isNil() {
        return new False();
    }
}
