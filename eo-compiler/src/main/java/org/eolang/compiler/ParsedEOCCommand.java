package org.eolang.compiler;

class ParsedEOCCommand {
    private final EOCCommandName name;

    public ParsedEOCCommand(EOCCommandName name) {
        this.name = name;
    }

    public EOCCommand withArgument(EOCCommandArgument argument) throws Exception {
        switch (this.name.string()) {
            case "":
                return new NullCommand();
            case "--help":
                return new HelpCommand(argument);
            case "--demo":
                return new DemoCommand(argument, "org\\eolang\\compiler\\eo" );
            default:
                throw new Exception();
        }
    }
}
