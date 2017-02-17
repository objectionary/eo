package org.eolang.compiler;

class DemoCommand implements EOCCommand {
    private final EOCCommandArgument filename;
    private final String path;

    public DemoCommand(EOCCommandArgument filename, String resourcePath) {
        this.filename = filename;
        this.path = resourcePath;
    }

    public String output() throws Exception {
        if (filename.isEmpty()) {
            return
                "Use --demo <filename> to see one of the following files compiled:\n"
                + new EOResourceFiles(path).formattedNames();
        } else {
            return new EOResourceFiles(path).eo(filename.string())
                + "\n"
                + new EOResourceFiles(path).java(filename.string());        }
    }
}
