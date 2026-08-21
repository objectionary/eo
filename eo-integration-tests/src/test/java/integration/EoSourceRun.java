/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.cactoos.Proc;

/**
 * Execution of EO source.
 *
 * <p>The sandbox compiles the {@code .eo} sources of the eo-runtime from
 * scratch, so it runs the merge the runtime's own build runs. A member that
 * reaches its receiver through {@code ^} has no rho until the merge gives it
 * one, and unmerged it is applied to the receiver through the first void it
 * does not have any more, which shifts every argument by one.</p>
 *
 * @since 0.56.3
 */
final class EoSourceRun implements Proc<Object> {

    /**
     * Name of the file the forked program reads its standard input from.
     */
    private static final String STDIN = "stdin.txt";

    /**
     * Fake maven reactor.
     */
    private final Farea farea;

    /**
     * What to feed the program through its standard input, empty for
     * nothing.
     */
    private final String input;

    /**
     * Ctor.
     * @param maven Fake maven reactor
     */
    EoSourceRun(final Farea maven) {
        this(maven, "");
    }

    /**
     * Ctor.
     * @param maven Fake maven reactor
     * @param stdin What to feed the program through its standard input
     */
    EoSourceRun(final Farea maven, final String stdin) {
        this.farea = maven;
        this.input = stdin;
    }

    @Override
    public void exec(final Object args) throws IOException {
        new RuntimeSources(
            Paths.get(System.getProperty("basedir", System.getProperty("user.dir")))
                .getParent()
                .resolve("eo-runtime")
                .resolve("src")
                .resolve("main")
                .resolve("eo")
        ).exec(this.farea);
        new EoMavenPlugin(this.farea)
            .appended()
            .execution("compile")
            .phase("generate-sources")
            .goals("register", "compile", "merge", "transpile")
            .configuration()
            .set("failOnWarning", "false")
            .set("offline", "true")
            .set("skipLinting", "true");
        if (this.input.isEmpty()) {
            this.farea.build()
                .plugins()
                .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                .execution("run")
                .phase("compile")
                .goals("java")
                .configuration()
                .set("mainClass", "org.eolang.Main")
                .set("arguments", args);
        } else {
            this.farea.files()
                .file(EoSourceRun.STDIN)
                .write(this.input.getBytes(StandardCharsets.UTF_8));
            this.farea.build()
                .plugins()
                .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                .execution("run")
                .phase("compile")
                .goals("exec")
                .configuration()
                .set("executable", "java")
                .set("inputFile", EoSourceRun.STDIN)
                .set("arguments", EoSourceRun.forked(args));
        }
        this.farea.exec("clean", "compile");
    }

    /**
     * The command line of a forked JVM running {@code Main} with the given
     * arguments. The {@code java} goal runs in Maven's own process, whose
     * standard input the program cannot be given, so a snippet that reads
     * from the console has to be forked by the {@code exec} goal instead.
     * @param args Arguments for the program
     * @return Arguments for the {@code exec} goal
     */
    private static Collection<String> forked(final Object args) {
        final List<String> line = new ArrayList<>(
            Arrays.asList("-cp", "%classpath", "org.eolang.Main")
        );
        if (args instanceof Iterable) {
            for (final Object arg : (Iterable<?>) args) {
                line.add(arg.toString());
            }
        } else if (args != null) {
            line.add(args.toString());
        }
        return line;
    }
}
