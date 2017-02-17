package org.eolang.compiler;

class NullCommand implements EOCCommand {
    public String output() {
        return "Usage: --help";
    }
}
