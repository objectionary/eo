/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.io.InputOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjAtomsTable}.
 *
 * @since 0.57
 */
@ExtendWith(MktmpResolver.class)
final class MjAtomsTableTest {

    @Test
    void generatesAtomsTableFromXmir(@Mktmp final Path temp) throws Exception {
        new Saved(
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > thing",
                        "  [] > is-good /bool",
                        "    ? > x",
                        "  [] > size /number"
                    )
                )
            ).parsed().toString(),
            temp.resolve("xmir/foo/thing.xmir")
        ).value();
        new FakeMaven(temp)
            .with("sources", temp.resolve("xmir").toFile())
            .with("csv", temp.resolve("classes/org/eolang/atoms.csv").toFile())
            .execute(MjAtomsTable.class);
        MatcherAssert.assertThat(
            "Generated CSV must contain entries for every declared atom",
            Files.readString(temp.resolve("classes/org/eolang/atoms.csv")),
            Matchers.allOf(
                Matchers.containsString("Φ.foo.thing.is-good,Φ.bool"),
                Matchers.containsString("Φ.foo.thing.size,Φ.number")
            )
        );
    }

    @Test
    void writesTableWhenOutputHasNoParentDirectory(@Mktmp final Path temp) throws Exception {
        final Path output = Path.of("mjatomstable-6447-test.csv");
        try {
            new FakeMaven(temp)
                .with("sources", temp.resolve("nothing").toFile())
                .with("csv", output.toFile())
                .execute(MjAtomsTable.class);
            MatcherAssert.assertThat(
                "A parentless csv must not throw and must produce the file",
                Files.exists(output),
                Matchers.is(true)
            );
        } finally {
            Files.deleteIfExists(output);
        }
    }

    @Test
    void writesEmptyTableWhenNoXmirSources(@Mktmp final Path temp) throws Exception {
        new FakeMaven(temp)
            .with("sources", temp.resolve("nothing").toFile())
            .with("csv", temp.resolve("classes/org/eolang/atoms.csv").toFile())
            .execute(MjAtomsTable.class);
        MatcherAssert.assertThat(
            "Output CSV should be empty when there are no XMIR sources",
            Files.readString(temp.resolve("classes/org/eolang/atoms.csv")),
            Matchers.emptyString()
        );
    }
}
