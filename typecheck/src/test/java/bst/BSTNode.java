package bst;

import bool.Bool;
import bool.False;
import nat.Nat;
import nat.Succ;
import nat.Zero;

public class BSTNode {
    public BSTNode() {}
    public /*abstract*/ Nat data() { return new Zero(); }
    public /*abstract*/ BSTNode left() { return new BSTNil(); }
    public /*abstract*/ BSTNode right() { return new BSTNil(); }
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

    public BSTNode insert(Nat id) {
        return (BSTNode) isNil().ifTrue(
                () -> new BSTInner(id, this, this),
                () -> id.lessThan(data())
                        .ifTrue(
                                () -> new BSTInner(data(), left().insert(id), right()),
                                () -> new BSTInner(data(), left(), right().insert(id))
                        )
                );
    }

    public BSTNode delete(Nat id) {
        return (BSTNode) isNil().ifTrue(
                () -> this,
                () -> data().equal(id).ifTrue(
                        () -> left().isNil().ifTrue(
                                () -> right(),
                                () -> right().isNil().ifTrue(
                                        () -> left(),
                                        () -> new BSTInner(right().min(), left(), right().withoutMin())
                                )
                        ),
                        () -> id.lessThan(data()).ifTrue(
                                () -> new BSTInner(data(), left().delete(id), right()),
                                () -> new BSTInner(data(), left(), right().delete(id))
                        )
                )
        );
    }

    public Nat min() {
        return (Nat) isNonNil().ifTrue(
                () -> left().isNonNil().ifTrue(
                        () -> left().min(),
                        () -> data()
                ),
                () -> new Zero()
        );
    }

    public BSTNode withoutMin() {
        return (BSTNode) isNonNil().ifTrue(
                () -> left().isNonNil().ifTrue(
                        () -> new BSTInner(data(), left().withoutMin(), right()),
                        () -> right().isNonNil().ifTrue(
                                () -> right(),
                                () -> new BSTNil()
                        )
                ),
                () -> new BSTNil()
        );
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
