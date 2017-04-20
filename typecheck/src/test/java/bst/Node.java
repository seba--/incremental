package bst;

import bool.Bool;
import bool.False;
import nat.Nat;
import nat.Succ;
import nat.Zero;

public class Node {
    public Node() {}
    public /*abstract*/ Nat data() { return new Zero(); }
    public /*abstract*/ Node left() { return new Nil(); }
    public /*abstract*/ Node right() { return new Nil(); }
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

    public Node insert(Nat id) {
        return (Node) isNil().ifTrue(
                () -> new Inner(id, this, this),
                () -> id.lessThan(data())
                        .ifTrue(
                                () -> new Inner(data(), left().insert(id), right()),
                                () -> new Inner(data(), left(), right().insert(id))
                        )
                );
    }

    public Node delete(Nat id) {
        return (Node) isNil().ifTrue(
                () -> this,
                () -> data().equal(id).ifTrue(
                        () -> left().isNil().ifTrue(
                                () -> right(),
                                () -> right().isNil().ifTrue(
                                        () -> left(),
                                        () -> new Inner(right().min(), left(), right().withoutMin())
                                )
                        ),
                        () -> id.lessThan(data()).ifTrue(
                                () -> new Inner(data(), left().delete(id), right()),
                                () -> new Inner(data(), left(), right().delete(id))
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

    public Node withoutMin() {
        return (Node) isNonNil().ifTrue(
                () -> left().isNonNil().ifTrue(
                        () -> new Inner(data(), left().withoutMin(), right()),
                        () -> right().isNonNil().ifTrue(
                                () -> right(),
                                () -> new Nil()
                        )
                ),
                () -> new Nil()
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
