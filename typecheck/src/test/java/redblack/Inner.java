package redblack;

import bool.Bool;
import bool.False;
import nat.Nat;

public class Inner extends Node {
    public Nat data;
    public Node left;
    public Node right;
    public Color color;
    public Inner(Color color, Nat data, Node left, Node right) {
        super();
        this.data = data;
        this.left = left;
        this.right = right;
        this.color = color;
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
    public Color color() {
        return this.color;
    }

    @Override
    public Bool isNil() {
        return new False();
    }

    public Node withBlackColor() {
        return new Inner(new Color(new False()), data(), left(), right());
    }
}
