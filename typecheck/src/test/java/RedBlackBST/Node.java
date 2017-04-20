package RedBlackBST;

import RedBlackBST.*;
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
    public abstract Bool color();
    public abstract Bool isNil();
    public Bool isNonNil() {
        return isNil().not();
    }


    public Bool isRed() {
        return (Bool) isNil().ifTrue(
                () -> new False(),
                () -> color().equalB(new True()).ifTrue(
                        () -> new True(),
                        () -> new False()
                )
        );
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
                () -> new Inner(id, this, this, color()),
                () -> data().equal(id).ifTrue(
                        () -> right().isRed().and(left().isRed().not()).ifTrue(
                                () -> rotateLeft(),
                                () -> left().isRed().and(left().left().isRed()).ifTrue(
                                        () -> rotateRight(),
                                        () -> flipColors()
                                )
                        ),
                        () -> id.lessThan(data()).ifTrue(
                                () -> new Inner(data(), left().insert(id), right(), color()),
                                () -> new Inner(data(), left(), right().insert(id), color())
                        )
                      )
        );
    }

    public Node delete(Nat id) {
        return (Node) isNil().ifTrue(
                () -> this,
                () -> data().lessThan(id).ifTrue(
                        () -> left().isRed().not().and(left().left().isRed().not()).ifTrue(
                                () -> moveRedLeft(),
                                () ->  new Inner(data(), left().delete(id), right(), color())
                                ),
                        () -> left().isRed().ifTrue(
                                () -> rotateRight(),
                                () -> data().equal(id).and(right().isNil()).ifTrue(
                                        () -> new Nil(),
                                        () -> right().isRed().not().and(right().left().isRed().not()).ifTrue(
                                                () -> moveRedRight(),
                                                () -> data().equal(id).ifTrue(
                                                        () -> new Inner(data(), left(), right().min(), color()),
                                                        () -> new Inner(data(), left(), right().delete(id), color())
                                                )
                                                )
                                        )
                                )
                        )
        );
    }

    public Node min() {
        return (Node) isNonNil().ifTrue(
                () -> left().isNonNil().ifTrue(
                        () -> left().min(),
                        () -> data()
                ),
                () -> new Zero()
        );
    }

    public Node rotateLeft() {

    }

    public Node rotateRight() {

    }

    public Node flipColors(){

    }

    public Node moveRedLeft(){

    }
    public Node moveRedRight(){

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
