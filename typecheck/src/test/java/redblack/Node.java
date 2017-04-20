package redblack;

import bool.Bool;
import bool.False;
import bool.True;
import nat.Nat;
import nat.Succ;
import nat.Zero;

public abstract class Node {
    public Node() {}

    public abstract Nat data();
    public abstract Node left();
    public abstract Node right();
    public abstract Color color();

    public abstract Bool isNil();
    public Bool isNonNil() {
        return isNil().not();
    }

    public Bool find(Nat id) {
        return isNonNil()
                .and(data().equal(id)
                        .or((Bool) id.lessThan(data())
                                .ifTrue(
                                        () -> left().find(id),
                                        () -> right().find(id))));
    }

    public Node insert(Nat id) {
        return insertAux(id).withBlackColor();
    }

    public Inner insertAux(Nat id) {
        return (Inner) isNil().ifTrue(
                () -> new Inner(new Color(new True()), id, this, this),
                () -> id.lessThan(data()).ifTrue(
                        () -> balance(color(), data(), left().insertAux(id), right()),
                        () -> id.greaterThan(data()).ifTrue(
                                () -> balance(color(), data(), left(), right().insertAux(id)),
                                () -> this
                        )
                )
        );
    }

    public Inner balance(Color c, Nat data, Node left, Node right) {
        return (Inner) isBlackLRedLRed(c, left, right).ifTrue(
                    () -> new Inner(
                            new Color(new True()),
                            left.data(),
                            new Inner(
                                    new Color(new False()),
                                    left.left().data(),
                                    left.left().left(),
                                    left.left().right()),
                            new Inner(
                                    new Color(new False()),
                                    data,
                                    left.right(),
                                    right
                            )
                    ),
         () -> isBlackLRedRRed(c, left, right).ifTrue(
                        () -> new Inner(
                                new Color(new True()),
                                left.right().data(),
                                new Inner(
                                        new Color(new False()),
                                        left.data(),
                                        left.left(),
                                        left.right().left()),
                                new Inner(
                                        new Color(new False()),
                                        data,
                                        left.right().right(),
                                        right
                                )
                        ),
         () -> isBlackRRedLRed(c, left, right).ifTrue(
                 () -> new Inner(
                         new Color(new True()),
                         right.left().data(),
                         new Inner(
                                 new Color(new False()),
                                 data,
                                 left,
                                 right.left().left()),
                         new Inner(
                                 new Color(new False()),
                                 right.data(),
                                 right.left().right(),
                                 right.right()
                         )
                 ),
         () -> isBlackRRedRRed(c, left, right).ifTrue(
                 () -> new Inner(
                         new Color(new True()),
                         right.data(),
                         new Inner(
                                 new Color(new False()),
                                 data,
                                 left,
                                 right.left()),
                         new Inner(
                                 new Color(new False()),
                                 right.right().data(),
                                 right.right().left(),
                                 right.right().right()
                         )
                 ),
         () -> new Inner(c, data, left, right)
         ))));
    }

    public Bool isBlackLRedLRed(Color c, Node left, Node right) {
        return c.isBlack()
                .and(left.color().isRed())
                .and(left.left().color().isRed());
    }

    public Bool isBlackLRedRRed(Color c, Node left, Node right) {
        return c.isBlack()
                .and(left.color().isRed())
                .and(left.right().color().isRed());
    }

    public Bool isBlackRRedLRed(Color c, Node left, Node right) {
        return c.isBlack()
                .and(right.color().isRed())
                .and(right.left().color().isRed());
    }

    public Bool isBlackRRedRRed(Color c, Node left, Node right) {
        return c.isBlack()
                .and(right.color().isRed())
                .and(right.right().color().isRed());
    }

    public Nat height() {
        return (Nat) isNil().ifTrue(
                () -> new Zero(),
                () -> left().height().greaterThan(right().height()).ifTrue(
                        () -> new Succ(left().height()),
                        () -> new Succ(right().height())
                )
        );
    }

    public Nat size() {
        return (Nat) isNil().ifTrue(
                () -> new Zero(),
                () -> new Succ(left().size().plus(right().size()))
        );
    }

}
