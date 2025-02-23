/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
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
        new HmBase(src).save(
            "+rt foo:0.0.0\n\n[args] > main\n  \"0.0.0\" > @\n",
            Paths.get("foo/main.eo")
        );
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
            new HmBase(classes).exists(classes.relativize(out)),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "EO file should contain the correct version information, but it doesn't",
            new TextOf(new HmBase(classes).load(classes.relativize(out))).asString(),
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
            new HmBase(classes).exists(classes.relativize(out)),
            Matchers.is(false)
        );
    }

}
