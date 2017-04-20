package redblack;

import bool.Bool;

public class Color {
    public Bool isRed;
    public Color(Bool isRed) {
        this.isRed = isRed;
    }
    public Bool isRed() {
        return this.isRed;
    }
    public Bool isBlack() {
        return isRed().not();
    }
}
