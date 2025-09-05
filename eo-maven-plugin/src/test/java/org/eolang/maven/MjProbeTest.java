/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjProbe}.
 *
 * @since 0.28.11
 */
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class MjProbeTest {

    @Test
    void findsProbesViaOfflineHashFile(@Mktmp final Path temp) throws IOException {
        final String tag = "master";
        final String tags = "org/eolang/maven/commits/tags.txt";
        new Saved(
            new ResourceOf(tags),
            temp.resolve("tags.txt")
        ).value();
        final String expected = "11";
        MatcherAssert.assertThat(
            String.format(
                "Number of objects that we should find during the probing phase should be equal %s",
                expected
            ),
            new FakeMaven(temp)
                .with("hash", new ChCached(new ChText(temp.resolve("tags.txt"), tag)))
                .withProgram(MjProbeTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .probed(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void findsProbesInOyRemote(@Mktmp final Path temp) throws IOException {
        final String tag = "0.50.0";
        final String expected = "6";
        final String found = new FakeMaven(temp)
            .with("tag", tag)
            .with("objectionary", new OyRemote(new ChRemote(tag)))
            .withProgram(MjProbeTest.program())
            .execute(new FakeMaven.Probe())
            .programTojo()
            .probed();
        MatcherAssert.assertThat(
            String.format(
                "We should find %s objects in git repository with tag '%s', but %s found",
                expected, tag, found
            ),
            found,
            Matchers.equalTo(expected)
        );
    }

    private static String[] program() {
        return new String[]{
            "+package foo.x",
            "+also while\n",
            "# No comments.",
            "[] > main",
            "  QQ.io.stdout > @",
            "    QQ.txt.sprintf",
            "      \"I am %d years old\"",
            "      plus.",
            "        1337",
            "        228",
        };
    }
}
