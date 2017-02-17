package org.eolang.compiler;

class HelpCommand implements EOCCommand {
    private final EOCCommandArgument argument;

    public HelpCommand(EOCCommandArgument commandArgument) {
        this.argument = commandArgument;
    }

    public String output() throws Exception {
        if (!argument.isEmpty()) throw new Exception();
        return "Use --demo to list compilation examples.";
    }
}
