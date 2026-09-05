/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.cactoos.set.SetOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.AdditionalAnswers;
import org.mockito.Mockito;

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
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(this.caller(temp));
        new Probing(
            tojos,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> new SetOf<>(
                        "tuple.each",
                        "tuple.eachi",
                        "tuple.withouti"
                    )
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should register all siblings from the same package",
            new MapOf<String, Boolean>(
                new MapEntry<>("tuple.eachi", tojos.contains("tuple.eachi")),
                new MapEntry<>("tuple.withouti", tojos.contains("tuple.withouti"))
            ),
            Matchers.allOf(
                Matchers.hasEntry("tuple.eachi", true),
                Matchers.hasEntry("tuple.withouti", true)
            )
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
                    () -> new SetOf<>("foo", "bar")
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should register the object and its root-package sibling",
            new MapOf<String, Boolean>(
                new MapEntry<>("foo", tojos.contains("foo")),
                new MapEntry<>("bar", tojos.contains("bar"))
            ),
            Matchers.allOf(
                Matchers.hasEntry("foo", true),
                Matchers.hasEntry("bar", true)
            )
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
        final Objectionary observed = Mockito.mock(
            Objectionary.class,
            AdditionalAnswers.delegatesTo(
                new OyIndexed(
                    new Objectionary.Fake(),
                    new ObjectsIndex(
                        () -> new SetOf<>("foo", "bar", "baz")
                    )
                )
            )
        );
        new Probing(
            tojos,
            observed,
            true
        ).exec();
        Mockito.verify(observed).children("");
        MatcherAssert.assertThat(
            "A single completion should register every root object",
            new MapOf<String, Boolean>(
                new MapEntry<>("all", tojos.size() == 4),
                new MapEntry<>("foo", tojos.contains("foo")),
                new MapEntry<>("bar", tojos.contains("bar")),
                new MapEntry<>("baz", tojos.contains("baz"))
            ),
            Matchers.allOf(
                Matchers.hasEntry("all", true),
                Matchers.hasEntry("foo", true),
                Matchers.hasEntry("bar", true),
                Matchers.hasEntry("baz", true)
            )
        );
    }

    @Test
    void doesNotCompletePackageThatLocalSourcesProvide(
        @TempDir final Path temp
    ) throws IOException {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(this.caller(temp));
        tojos.add("tuple.each").withSource(temp.resolve("tuple").resolve("each.eo"));
        new Probing(tojos, this.tuples(), true).exec();
        MatcherAssert.assertThat(
            "Probe should not register the siblings of a package that is on disk already",
            tojos.contains("tuple.eachi") || tojos.contains("tuple.withouti"),
            Matchers.is(false)
        );
    }

    @Test
    void avoidsDuplicateTojoForAnOrgEolangPrefixedReference(
        @TempDir final Path temp
    ) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > test",
                    "  Q.org.eolang.number.gte > @"
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
                    () -> new SetOf<>("number", "number.gte", "number.sqrt", "number.abs")
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should not also track the sibling under its short, unqualified name",
            tojos.contains("number.gte"),
            Matchers.is(false)
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

    private Path caller(final Path temp) throws IOException {
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
        return xmir;
    }

    private Objectionary tuples() {
        return new OyIndexed(
            new Objectionary.Fake(),
            new ObjectsIndex(
                () -> new SetOf<>("tuple.each", "tuple.eachi", "tuple.withouti")
            )
        );
    }
}
