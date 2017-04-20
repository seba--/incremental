package bool;

import block.Block;

public abstract class Bool {
    public Bool() {}
    public abstract Bool not();
    public abstract Bool and(Bool that);
    public abstract Bool or(Bool that);
    public abstract Bool equalB(Bool that);
    public abstract Object ifTrue(Block thn, Block els);
    public Object ifFalse(Block thn, Block els) {
        return ifTrue(els, thn);
    }
}
