package RedBlackBST;

import RedBlackBST.*;
import bool.Bool;
import bool.False;
import nat.Nat;

public class Inner extends RedBlackBST.Node {
    public Nat data;
    public RedBlackBST.Node left;
    public RedBlackBST.Node right;
    public  Bool color;
    public Inner(Nat data, RedBlackBST.Node left, RedBlackBST.Node right, Bool color) {
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
    public RedBlackBST.Node left() {
        return this.left;
    }

    @Override
    public RedBlackBST.Node right() {
        return this.right;
    }

    @Override
    public Bool color() {
        return this.color;
    }

    @Override
    public Bool isNil() {
        return new False();
    }
}
