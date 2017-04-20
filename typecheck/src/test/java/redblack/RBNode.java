package redblack;

import bool.Bool;
import bool.False;
import bool.True;
import nat.Nat;
import nat.Succ;
import nat.Zero;

public class RBNode {
    public RBNode() {}

    public /*abstract*/ Nat data() { return new Zero(); }
    public /*abstract*/ RBNode left() { return new RBNil(); }
    public /*abstract*/ RBNode right() { return new RBNil(); }
    public /*abstract*/ Color color() { return new Color(new False()); }

    public /*abstract*/ Bool isNil() { return new False(); }
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

    public RBNode insert(Nat id) {
        return insertAux(id).withBlackColor();
    }

    public RBInner insertAux(Nat id) {
        return (RBInner) isNil().ifTrue(
                () -> new RBInner(new Color(new True()), id, this, this),
                () -> id.lessThan(data()).ifTrue(
                        () -> balance(color(), data(), left().insertAux(id), right()),
                        () -> id.greaterThan(data()).ifTrue(
                                () -> balance(color(), data(), left(), right().insertAux(id)),
                                () -> this
                        )
                )
        );
    }

    public RBInner balance(Color c, Nat data, RBNode left, RBNode right) {
        return (RBInner) isBlackLRedLRed(c, left, right).ifTrue(
                    () -> new RBInner(
                            new Color(new True()),
                            left.data(),
                            new RBInner(
                                    new Color(new False()),
                                    left.left().data(),
                                    left.left().left(),
                                    left.left().right()),
                            new RBInner(
                                    new Color(new False()),
                                    data,
                                    left.right(),
                                    right
                            )
                    ),
         () -> isBlackLRedRRed(c, left, right).ifTrue(
                        () -> new RBInner(
                                new Color(new True()),
                                left.right().data(),
                                new RBInner(
                                        new Color(new False()),
                                        left.data(),
                                        left.left(),
                                        left.right().left()),
                                new RBInner(
                                        new Color(new False()),
                                        data,
                                        left.right().right(),
                                        right
                                )
                        ),
         () -> isBlackRRedLRed(c, left, right).ifTrue(
                 () -> new RBInner(
                         new Color(new True()),
                         right.left().data(),
                         new RBInner(
                                 new Color(new False()),
                                 data,
                                 left,
                                 right.left().left()),
                         new RBInner(
                                 new Color(new False()),
                                 right.data(),
                                 right.left().right(),
                                 right.right()
                         )
                 ),
         () -> isBlackRRedRRed(c, left, right).ifTrue(
                 () -> new RBInner(
                         new Color(new True()),
                         right.data(),
                         new RBInner(
                                 new Color(new False()),
                                 data,
                                 left,
                                 right.left()),
                         new RBInner(
                                 new Color(new False()),
                                 right.right().data(),
                                 right.right().left(),
                                 right.right().right()
                         )
                 ),
         () -> new RBInner(c, data, left, right)
         ))));
    }

    public Bool isBlackLRedLRed(Color c, RBNode left, RBNode right) {
        return c.isBlack()
                .and(left.color().isRed())
                .and(left.left().color().isRed());
    }

    public Bool isBlackLRedRRed(Color c, RBNode left, RBNode right) {
        return c.isBlack()
                .and(left.color().isRed())
                .and(left.right().color().isRed());
    }

    public Bool isBlackRRedLRed(Color c, RBNode left, RBNode right) {
        return c.isBlack()
                .and(right.color().isRed())
                .and(right.left().color().isRed());
    }

    public Bool isBlackRRedRRed(Color c, RBNode left, RBNode right) {
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
