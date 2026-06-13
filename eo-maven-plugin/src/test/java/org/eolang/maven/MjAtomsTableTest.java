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

    @Test
    void generatesAtomsTableFromXmir(@Mktmp final Path temp) throws Exception {
        final String eo = String.join(
            "\n",
            "+package foo",
            "",
            "# Top object.",
            "[] > thing",
            "  # Atom returning bool.",
            "  [x] > is-good /bool",
            "  # Atom returning number.",
            "  [] > size /number"
        );
        final Path xmir = temp.resolve("xmir/foo/thing.xmir");
        new Saved(
            new EoSyntax(new InputOf(eo)).parsed().toString(),
            xmir
        ).value();
        final Path output = temp.resolve("classes/org/eolang/atoms.csv");
        new FakeMaven(temp)
            .with("atomsTableSourcesDir", temp.resolve("xmir").toFile())
            .with("atomsTableOutput", output.toFile())
            .execute(MjAtomsTable.class);
        MatcherAssert.assertThat(
            "Generated CSV must exist",
            Files.exists(output),
            Matchers.is(true)
        );
        final String content = Files.readString(output);
        MatcherAssert.assertThat(
            "Atom with bool return type must appear in the table",
            content,
            Matchers.containsString("Φ.foo.thing.is-good,Φ.bool")
        );
        MatcherAssert.assertThat(
            "Atom with number return type must appear in the table",
            content,
            Matchers.containsString("Φ.foo.thing.size,Φ.number")
        );
    }

    @Test
    void writesEmptyTableWhenNoXmirSources(@Mktmp final Path temp) throws Exception {
        final Path output = temp.resolve("classes/org/eolang/atoms.csv");
        new FakeMaven(temp)
            .with("atomsTableSourcesDir", temp.resolve("nothing").toFile())
            .with("atomsTableOutput", output.toFile())
            .execute(MjAtomsTable.class);
        MatcherAssert.assertThat(
            "Output CSV should be created even with no XMIR sources",
            Files.exists(output),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "Output CSV should be empty when there are no XMIR sources",
            Files.readString(output),
            Matchers.equalTo("")
        );
    }
}
