package bool;

import block.Block;

public class True extends Bool {
    public True() {
        super();
    }

    @Override
    public Bool not() {
        return new False();
    }

    @Override
    public Bool and(Bool that) {
        return that;
    }

    @Override
    public Bool or(Bool that) {
        return this;
    }

    @Override
    public Bool equalB(Bool that) {
        return that;
    }

    @Override
    public Object ifTrue(Block thn, Block els) {
        return thn.execute();
    }
}
