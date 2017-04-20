package bool;

import block.Block;

public class Bool {
    public Bool() {}
    public /*abstract*/ Bool not() { return new False(); }
    public /*abstract*/ Bool and(Bool that) { return new False(); }
    public /*abstract*/ Bool or(Bool that) { return new False(); }
    public /*abstract*/ Object ifTrue(Block thn, Block els) { return new False(); }
    public Object ifFalse(Block thn, Block els) {
        return ifTrue(els, thn);
    }
}
