package bst;

import bool.Bool;
import bool.True;
import nat.Nat;
import nat.Zero;

public class Nil extends Node {
    public Nil() {
        super();
    }

    @Override
    public Nat data() {
        return new Zero();
    }

    @Override
    public Node left() {
        return this;
    }

    @Override
    public Node right() {
        return this;
    }

    @Override
    public Bool isNil() {
        return new True();
    }
}
