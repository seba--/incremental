package redblack;

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

    public static Node mkRedBlack(int... is) {
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
        Node tree = mkRedBlack(5, 7, 3, 1, 9, 4, 6, 11, 12, 13, 14, 15, 16, 17, 18);
        System.out.println(toString(tree));
        System.out.println("size = " + toString(tree.size()));
        System.out.println("height = " + toString(tree.height()));
    }
}
