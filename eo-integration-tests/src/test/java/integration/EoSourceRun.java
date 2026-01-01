/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
import org.cactoos.Proc;

/**
 * Execution of EO source.
 * @since 0.56.3
 * @todo #4232:30min Conditionally add JUnit dependencies to integration tests.
 *  JUnit dependencies are needed for EO programs that contain test attributes
 *  (methods starting with '+'), but not for programs without tests. The XSL
 *  template now conditionally generates JUnit imports, but integration tests
 *  still need JUnit dependencies available when processing EO programs with tests.
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
        this.farea.build()
            .plugins()
            .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
            .execution("run")
            .phase("test")
            .goals("java")
            .configuration()
            .set("mainClass", "org.eolang.Main")
            .set("arguments", args);
        this.farea.exec("clean", "test");
    }
}
