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
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link MjProbe}.
 *
 * @since 0.28.11
 * @todo #4203:90min Make {@link MjProbeTest} To Check Probes
 *  Currently, this test case only checks the number of probes found
 *  but does not verify the actual content of the probes.
 *  It would be beneficial to enhance this test to
 *  check the actual probes found.
 * @todo #4203:90min Enable {@link MjProbeTest#findsProbesInSimpleProgram(Path)} Test.
 *  This test is currently disabled because it fails.
 *  We should investigate the cause of the failure and enable the test.
 *  Originally this issue affected the integration tests,
 *  <a href="https://github.com/objectionary/eo/issues/4203">here</a>
 */
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class MjProbeTest {
    @Test
    void findsProbes(@Mktmp final Path temp) throws Exception {
        final String expected = "11";
        MatcherAssert.assertThat(
            String.format(
                "Number of objects that we should find during the probing phase should be equal %s",
                expected
            ),
            new FakeMaven(temp)
                .with("foreignFormat", "json")
                .withProgram(MjProbeTest.program())
                .execute(new FakeMaven.Probe())
                .programTojo()
                .probed(),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/probe-packs/", glob = "**.yaml")
    void checksProbePacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtYaml(
                    yaml,
                    eo -> new EoSyntax(
                        new InputOf(String.format("%s\n", eo))
                    ).parsed()
                )
            ),
            new XtoryMatcher()
        );
    }

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
    @Disabled
    void findsProbesInSimpleProgram(@Mktmp final Path temp) throws IOException {
        final String found = new FakeMaven(temp)
            .withProgram(
                "+alias QQ.io.stdout\n",
                "stdout \"Hello, world!\" > simple"
            )
            .execute(new FakeMaven.Probe())
            .programTojo().probed();
        final String expected = "6";
        MatcherAssert.assertThat(
            String.format(
                "We should find %s objects in simple program, but %s found",
                expected, found
            ),
            found,
            Matchers.equalTo(expected)
        );
    }

    @Test
    void findsProbesInSimpleProgramWithComment(@Mktmp final Path temp) throws IOException {
        final String found = new FakeMaven(temp)
            .withProgram(
                "# No comments.",
                "[] > simple",
                "  QQ.io.stdout > @",
                "    \"Hello, world!\""
            ).execute(new FakeMaven.Probe())
            .programTojo()
            .probed();
        final String expected = "6";
        MatcherAssert.assertThat(
            String.format(
                "We should find %s objects in simple program without comments, but %s found",
                expected, found
            ),
            found,
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
