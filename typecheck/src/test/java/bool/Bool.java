package bool;

import block.Block;

public class Bool {
    public Bool() {}
<<<<<<< HEAD
    public abstract Bool not();
    public abstract Bool and(Bool that);
    public abstract Bool or(Bool that);
    public abstract Object ifTrue(Block thn, Block els);
=======
>>>>>>> 266f569b25f1194043c8296dcd91c70fd2c47325
    public /*abstract*/ Bool not() { return new False(); }
    public /*abstract*/ Bool and(Bool that) { return new False(); }
    public /*abstract*/ Bool or(Bool that) { return new False(); }
    public /*abstract*/ Object ifTrue(Block thn, Block els) { return new False(); }
    public Object ifFalse(Block thn, Block els) {
        return ifTrue(els, thn);
    }
}
