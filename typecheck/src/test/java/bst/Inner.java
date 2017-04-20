package bst;

import bool.Bool;
import bool.False;
import nat.Nat;

public class Inner extends Node {
    public Nat data;
    public Node left;
    public Node right;
    public Inner(Nat data, Node left, Node right) {
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
    public Node left() {
        return this.left;
    }

    @Override
    public Node right() {
        return this.right;
    }

    @Override
    public Bool isNil() {
        return new False();
    }
}
