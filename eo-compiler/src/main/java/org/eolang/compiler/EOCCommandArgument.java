package org.eolang.compiler;

class EOCCommandArgument {
    private final String[] args;

    public EOCCommandArgument(String[] args) {
        this.args = args;
    }

    public String string() {
        return isEmpty() ? "" : this.args[1];
    }

    public boolean isEmpty() {
        return args.length < 2;
    }
}
