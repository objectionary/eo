/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Probing}.
 * @since 0.67.0
 * @todo #5084:30min Create a reusable test helper that represents a standard EO program
 *  (like the hello-world one) for use across tests like {@link ProbeTest} and
 *  {@link MjProbeTest} and {@link FakeMaven#withHelloWorld()}, so we don't duplicate
 *  the EO source inline in multiple places.
 */
final class ProbeTest {

    @Test
    void probesSuccessfully(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "+alias stdout org.eolang.io.stdout",
                    "+home https://www.eolang.org",
                    "+package foo.x",
                    "+unlint object-has-data",
                    "+version 0.0.0",
                    "",
                    "# No comments.",
                    "[x] > main",
                    "  (stdout \"Hello!\" x).print > @"
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(tojos, new Objectionary.Fake(), true).exec();
        MatcherAssert.assertThat(
            "Probe should have found and registered objects from the objectionary",
            tojos.size(),
            Matchers.equalTo(8)
        );
    }

    @Test
    void skipsWhenOffline(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "# No comments.",
                    "[] > test",
                    "  Q.io.stdout > @",
                    "    \"Hello!\""
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(tojos, new Objectionary.Fake(), false).exec();
        MatcherAssert.assertThat(
            "Probe should not register any objects when offline",
            tojos.size(),
            Matchers.equalTo(1)
        );
    }
}
