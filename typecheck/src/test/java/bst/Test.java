package bst;

import bool.True;
import nat.Nat;
import nat.Succ;
import nat.Zero;

public class Test {
    public static Nat mkNat(int i) {
        if (i == 0) return new Zero();
        else if (i > 0) return new Succ(mkNat(i - 1));
        else throw new IllegalArgumentException();
    }

    public static Node mkBST(int... is) {
        Node current = new Nil();
        for (int i : is) {
            current = current.insert(mkNat(i));
        }
        return current;
    }

    public static String toString(Nat n) {
        Nat current = n;
        int i = 0;
        while (current.isNonZero() instanceof True) {
            i += 1;
            current = current.pred();
        }
        return Integer.toString(i);
    }

    public static String toString(Node n) {
        if (n.isNil() instanceof True) return "[]";
        else return "[data:" + toString(n.data()) + ", left:" + toString(n.left()) + ", right:" + toString(n.right()) + "]";
    }



    public static void main(String[] args) {
        Node bst = mkBST(5, 7, 3, 1, 9, 4, 6);
        System.out.println(toString(bst));
        System.out.println("size = " + toString(bst.size()));
        System.out.println("height = " + toString(bst.height()));
    }
}
