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
 * @since 0.57
 */
@ExtendWith(MktmpResolver.class)
final class MjAtomsTableTest {

    /**
     * Execute the parentless output scenario in an isolated working directory.
     * @param args Temporary directory path
     * @throws Exception If execution fails
     */
    public static void main(final String... args) throws Exception {
        final Path temp = Path.of(args[0]);
        new FakeMaven(temp)
            .with("atomsInputDir", temp.resolve("xmir").toFile())
            .with("atomsOutput", Path.of("atoms.csv").toFile())
            .execute(MjAtomsTable.class);
    }

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
            .with("atomsInputDir", temp.resolve("xmir").toFile())
            .with("atomsOutput", temp.resolve("classes/org/eolang/atoms.csv").toFile())
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
    void writesEmptyTableWhenNoXmirSources(@Mktmp final Path temp) throws Exception {
        new FakeMaven(temp)
            .with("atomsInputDir", temp.resolve("nothing").toFile())
            .with("atomsOutput", temp.resolve("classes/org/eolang/atoms.csv").toFile())
            .execute(MjAtomsTable.class);
        MatcherAssert.assertThat(
            "Output CSV should be empty when there are no XMIR sources",
            Files.readString(temp.resolve("classes/org/eolang/atoms.csv")),
            Matchers.emptyString()
        );
    }

    @Test
    void writesAtomsTableToParentlessFile(@Mktmp final Path temp) throws Exception {
        new Saved(
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > thing",
                        "  [] > size /number"
                    )
                )
            ).parsed().toString(),
            temp.resolve("xmir/foo/thing.xmir")
        ).value();
        MatcherAssert.assertThat(
            "Parentless output must be written in an isolated directory",
            String.format(
                "%d:%s",
                new ProcessBuilder(
                    Path.of(
                        System.getProperty("java.home"), "bin", "java"
                    ).toString(),
                    "-cp",
                    System.getProperty("java.class.path"),
                    MjAtomsTableTest.class.getName(),
                    temp.toString()
                ).directory(temp.toFile()).inheritIO().start().waitFor(),
                Files.readString(temp.resolve("atoms.csv"))
            ),
            Matchers.equalTo(
                String.format("0:Φ.foo.thing.size,Φ.number%c", '\n')
            )
        );
    }
}
