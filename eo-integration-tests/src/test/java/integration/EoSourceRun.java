/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
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
