/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.bytes.BytesOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CopyMojo}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class CopyMojoTest {

    @Test
    void copiesSources(@Mktmp final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        final Path classes = temp.resolve("classes");
        new Saved(
            "+rt foo:0.0.0\n\n[args] > main\n  \"0.0.0\" > @\n",
            src.resolve("foo/main.eo")
        ).value();
        final String ver = "1.1.1";
        new FakeMaven(temp)
            .with("sourcesDir", src.toFile())
            .with("outputDir", classes.toFile())
            .with("skip", false)
            .with("version", ver)
            .execute(CopyMojo.class);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            "Expected EO source file to be copied, but it was not found",
            Files.exists(out),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "EO file should contain the correct version information, but it doesn't",
            new TextOf(new BytesOf(Files.readAllBytes(out))).asString(),
            Matchers.allOf(
                Matchers.containsString("+rt foo:"),
                Matchers.containsString("0.0.0"),
                Matchers.containsString(ver)
            )
        );
    }

    @Test
    void skipsCopyMojo(@Mktmp final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        new FakeMaven(temp)
            .withProgram(
                "+rt foo:0.0.0",
                "",
                "",
                "[args] > main",
                "  \"0.0.0\" > @"
            )
            .with("sourcesDir", temp.toFile())
            .with("outputDir", temp.resolve("classes").toFile())
            .with("skip", true)
            .with("version", "1.1.1")
            .execute(CopyMojo.class);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
            "CopyMojo must skip copying, but it doesn't",
            Files.exists(out),
            Matchers.is(false)
        );
    }

}
