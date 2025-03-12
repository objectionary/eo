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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MarkMojo}.
 *
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class MarkMojoTest {
    /**
     * Version.
     */
    private static final String VERSION = "0.28.0";

    /**
     * The message that versions must match.
     */
    private static final String MESSAGE = "Versions must match, but they don't";

    @Test
    void extendsForeignWithNewObjects(@Mktmp final Path temp) throws IOException {
        MarkMojoTest.source(temp);
        final FakeMaven maven = new FakeMaven(temp);
        maven.execute(MarkMojo.class);
        MatcherAssert.assertThat(
            MarkMojoTest.MESSAGE,
            maven.foreignTojos()
                .all()
                .iterator()
                .next()
                .version(),
            Matchers.equalTo(MarkMojoTest.VERSION)
        );
    }

    @Test
    void updatesVersionIfItExists(@Mktmp final Path temp) throws IOException {
        MarkMojoTest.source(temp);
        final FakeMaven maven = new FakeMaven(temp);
        final TjsForeign foreign = maven.foreignTojos();
        foreign.add("foo.bar")
            .withVersion("*.*.*");
        maven.execute(MarkMojo.class);
        MatcherAssert.assertThat(
            MarkMojoTest.MESSAGE,
            foreign.all().iterator().next().version(),
            Matchers.equalTo(MarkMojoTest.VERSION)
        );
        MatcherAssert.assertThat(
            "Size must equal 1, but it doesn't",
            foreign.size(),
            Matchers.equalTo(1)
        );
    }

    private static void source(final Path temp) throws IOException {
        new Home(temp.resolve("target").resolve(ResolveMojo.DIR)).save(
            "hi",
            Paths.get(
                String.format(
                    "foo/hello/-/%s/%s/foo/bar.eo", MarkMojoTest.VERSION, CopyMojo.DIR
                )
            )
        );
    }
}
