/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.set.SetOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Probing}.
 * @since 0.67.0
 */
final class ProbingTest {

    @Test
    void probesSuccessfully(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                new HelloWorld().asString()
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(tojos, new Objectionary.Fake(), true).exec();
        MatcherAssert.assertThat(
            "Probe should have found and registered objects from the objectionary",
            tojos.size(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void completesPartiallyProbedPackage(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "+package foo",
                    "",
                    "[] > test",
                    "  Q.tuple.each > @"
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(
            tojos,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> new SetOf<>(
                        "tuple.each",
                        "tuple.eachi",
                        "tuple.withouti",
                        "tuple.nested.object"
                    )
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should register only direct siblings from the same package",
            tojos.contains("tuple.eachi")
                && tojos.contains("tuple.withouti")
                && !tojos.contains("tuple.nested.object"),
            Matchers.is(true)
        );
    }

    @Test
    void completesRootPackage(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > test",
                    "  Q.foo > @"
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(
            tojos,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> new SetOf<>("foo", "bar", "nested.object")
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should register the root sibling but not a nested object",
            tojos.contains("foo")
                && tojos.contains("bar")
                && !tojos.contains("nested.object"),
            Matchers.is(true)
        );
    }

    @Test
    void completesRootPackageOnce(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > test",
                    "  Q.foo > first",
                    "  Q.baz > @"
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(xmir);
        new Probing(
            tojos,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> new SetOf<>("foo", "bar", "baz", "nested.object")
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Multiple root probes should produce one complete root object set",
            tojos.size() == 4
                && tojos.contains("foo")
                && tojos.contains("bar")
                && tojos.contains("baz"),
            Matchers.is(true)
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
