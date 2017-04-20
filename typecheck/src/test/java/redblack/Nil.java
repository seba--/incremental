package redblack;

import bool.Bool;
import bool.False;
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
    public Color color() {
        return new Color(new False());
    }

    @Override
    public Bool isNil() {
        return new True();
    }
}
