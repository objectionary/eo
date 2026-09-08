/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
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
 * <p>A snippet that reads from the console is run in a JVM of its own, with
 * its standard input coming from a file: {@code console.read} reads the real
 * file descriptor 0, and neither the exec plugin nor {@code Farea} can give
 * a process an input of its own.</p>
 *
 * <p>That JVM is given a stack of its own too. Dataizing an EO object walks
 * the whole chain of its nested objects on the Java stack, and a recursive
 * object adds its own chain on every step, so the depth grows with the
 * input and not with the length of the program. Every other JVM this
 * project starts to run EO is handed the same flag, {@code JarIT} and the
 * {@code argLine} of both Surefire and Failsafe among them; on the default
 * stack a snippet reading a few hundred lines already dies with a
 * {@link StackOverflowError}.</p>
 *
 * @since 0.56.3
 */
final class EoSourceRun implements Proc<Object> {

    /**
     * Name of the file the forked program reads its standard input from.
     */
    private static final String STDIN = "stdin.txt";

    /**
     * Name of the file the dependency plugin writes the classpath into.
     */
    private static final String CLASSPATH = "target/cp.txt";

    /**
     * Fake maven reactor.
     */
    private final Farea farea;

    /**
     * Home of the fake project, where the sources and the build land.
     */
    private final Path home;

    /**
     * What to feed the program through its standard input, empty for
     * nothing.
     */
    private final String input;

    /**
     * What the forked program printed, empty when nothing was forked.
     */
    private final AtomicReference<String> printed;

    /**
     * Ctor.
     * @param maven Fake maven reactor
     */
    EoSourceRun(final Farea maven) {
        this(maven, Paths.get("."), "");
    }

    /**
     * Ctor.
     * @param maven Fake maven reactor
     * @param dir Home of the fake project
     * @param stdin What to feed the program through its standard input
     */
    EoSourceRun(final Farea maven, final Path dir, final String stdin) {
        this.farea = maven;
        this.home = dir;
        this.input = stdin;
        this.printed = new AtomicReference<>("");
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
            this.farea.exec("clean", "compile");
        } else {
            this.farea.files()
                .file(EoSourceRun.STDIN)
                .write(this.input.getBytes(StandardCharsets.UTF_8));
            this.farea.build()
                .plugins()
                .append("org.apache.maven.plugins", "maven-dependency-plugin", "3.6.1")
                .execution("classpath")
                .phase("compile")
                .goals("build-classpath")
                .configuration()
                .set("outputFile", EoSourceRun.CLASSPATH);
            this.farea.exec("clean", "compile");
            this.printed.set(this.forked(args));
        }
    }

    /**
     * What the forked program printed on its standard output and error.
     * @return The output, empty when the program was not forked
     */
    String output() {
        return this.printed.get();
    }

    private String forked(final Object args) throws IOException {
        final List<String> line = new ArrayList<>(
            Arrays.asList(
                ProcessHandle.current().info().command().orElse("java"),
                "-Xss64M",
                "-cp",
                String.format(
                    "%s%s%s",
                    this.home.resolve("target/classes"),
                    File.pathSeparator,
                    new String(
                        Files.readAllBytes(this.home.resolve(EoSourceRun.CLASSPATH)),
                        StandardCharsets.UTF_8
                    ).trim()
                ),
                "org.eolang.Main"
            )
        );
        for (final Object arg : (Iterable<?>) args) {
            line.add(arg.toString());
        }
        final Process proc = new ProcessBuilder(line)
            .directory(this.home.toFile())
            .redirectInput(this.home.resolve(EoSourceRun.STDIN).toFile())
            .redirectErrorStream(true)
            .start();
        try (InputStream stream = proc.getInputStream()) {
            final String out = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            proc.waitFor();
            return out;
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IOException(ex);
        } finally {
            proc.destroy();
        }
    }
}
