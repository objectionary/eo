package org.eolang.compiler;

class EOCCommandName {
    private final String[] args;

    public EOCCommandName(String[] args) {
        this.args = args;
    }

    public String string() {
        if (this.doesNotExist())
            return "";
        else
            return this.args[0];
    }

    public boolean doesNotExist() {
        return args.length < 1;
    }
}
