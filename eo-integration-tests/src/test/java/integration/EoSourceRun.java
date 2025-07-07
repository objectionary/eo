/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.cactoos.Proc;

/**
 * Execution of EO source.
 * @since 0.56.3
 */
final class EoSourceRun implements Proc<Object> {

    /**
     * Fake maven reactor.
     */
    private final Farea farea;

    /**
     * Ctor.
     * @param maven Fake maven reactor
     */
    EoSourceRun(final Farea maven) {
        this.farea = maven;
    }

    @Override
    public void exec(final Object args) throws IOException {
        new EoMavenPlugin(this.farea)
            .appended()
            .execution("compile")
            .phase("generate-sources")
            .goals("register", "compile", "transpile")
            .configuration()
            .set("failOnWarning", Boolean.FALSE.toString())
            .set("skipLinting", Boolean.TRUE.toString());
        this.farea.exec("clean", "generate-sources");
        if (this.testsPresent()) {
            this.farea.dependencies().append(
                "org.junit.jupiter",
                "junit-jupiter-engine",
                "5.10.3"
            );
            this.farea.dependencies().append(
                "org.junit.jupiter",
                "junit-jupiter-params",
                "5.10.3"
            );
            this.farea.dependencies().append(
                "org.junit.jupiter",
                "junit-jupiter-api",
                "5.10.3"
            );
            this.farea.dependencies().append(
                "org.junit-pioneer",
                "junit-pioneer",
                "2.2.0"
            );
        }
        this.farea.build()
            .plugins()
            .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
            .execution("run")
            .phase("test")
            .goals("java")
            .configuration()
            .set("mainClass", "org.eolang.Main")
            .set("arguments", args);
        this.farea.exec("test");
    }

    /**
     * Check if tests are present in generated test sources.
     * @return True if test sources contain JUnit tests, false otherwise
     * @throws IOException If I/O fails
     */
    private boolean testsPresent() throws IOException {
        final Path testdir = this.farea.files().path().resolve("target/generated-test-sources");
        if (!Files.exists(testdir)) {
            return false;
        }
        try (Stream<Path> paths = Files.walk(testdir)) {
            return paths
                .filter(path -> path.toString().endsWith(".java"))
                .anyMatch(path -> {
                    try {
                        return Files.readString(path).contains("@Test");
                    } catch (final IOException ex) {
                        return false;
                    }
                });
        }
    }
}
