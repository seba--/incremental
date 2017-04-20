package bool;

import block.Block;

public class False extends Bool {
    public False() {
        super();
    }

    @Override
    public Bool not() {
        return new True();
    }

    @Override
    public Bool and(Bool that) {
        return this;
    }

    @Override
    public Bool or(Bool that) {
        return that;
    }

    @Override
    public Object ifTrue(Block thn, Block els) {
        return els.execute();
    }
}
