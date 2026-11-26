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
 *
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
        new Probing(tojos, this.tuples(), true).exec();
        MatcherAssert.assertThat(
            "Probe should have registered the siblings that were never probed directly",
            tojos.contains("tuple.eachi") && tojos.contains("tuple.withouti"),
            Matchers.is(true)
        );
    }

    @Test
    void completesRootPackageForTopLevelProbe(@TempDir final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "+package foo",
                    "",
                    "[] > test",
                    "  Q.tuple > @"
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
                    () -> new SetOf<>("tuple", "seq", "tuple.each")
                )
            ),
            true
        ).exec();
        MatcherAssert.assertThat(
            "Probe should have registered the top-level sibling of a bare probed object",
            tojos.contains("seq"),
            Matchers.is(true)
        );
    }

    @Test
    void registersObjectsInsideSiblingThatNoSourceNames(
        @TempDir final Path temp
    ) throws IOException {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(this.number(temp));
        new Probing(tojos, this.integers(), true).exec();
        MatcherAssert.assertThat(
            "Probe should have registered the objects deep inside a sibling, reached only by dispatch",
            tojos.contains("org.eolang.i64.div") && tojos.contains("org.eolang.i64.div.exact"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotRegisterObjectsInsideSiblingThatLocalSourcesProvide(
        @TempDir final Path temp
    ) throws IOException {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("test").withXmir(this.number(temp));
        tojos.add("org.eolang.i64").withSource(temp.resolve("i64.eo"));
        new Probing(tojos, this.integers(), true).exec();
        MatcherAssert.assertThat(
            "Probe should not register the objects inside a sibling that is on disk already",
            tojos.contains("org.eolang.i64.div"),
            Matchers.is(false)
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

    private Path number(final Path temp) throws IOException {
        final Path xmir = temp.resolve("test.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > test",
                    "  Q.org.eolang.number > @"
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        return xmir;
    }

    private Objectionary integers() {
        return new OyIndexed(
            new Objectionary.Fake(),
            new ObjectsIndex(
                () -> new SetOf<>("number", "i64", "i64.div", "i64.div.exact")
            )
        );
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
