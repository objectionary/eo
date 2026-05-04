/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Scalar;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjProbe}.
 * @since 0.28.11
 */
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class MjProbeTest {

    @Test
    void findsProbesViaOfflineHashFile(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            temp.resolve("tags.txt")
        ).value();
        final String expected = "7";
        MatcherAssert.assertThat(
            String.format(
                "Number of objects that we should find during the probing phase should be equal %s",
                expected
            ),
            new FakeMaven(temp)
                .with("hash", new ChCached(new ChText(temp.resolve("tags.txt"), "0.23.15")))
                .withHelloWorld()
                .execute(new FakeMaven.Probe())
                .programTojo()
                .probed(),
            Matchers.equalTo(expected)
        );
    }

    /**
     * Finds probes in objectionary remote repository.
     * @param temp Temporary folder
     * @throws IOException If some problem inside
     */
    @Test
    void findsProbesInOyRemote(@Mktmp final Path temp) throws IOException {
        final String tag = "0.23.15";
        final String expected = "3";
        final String found = new FakeMaven(temp)
            .with("tag", tag)
            .with(
                "objectionary",
                (Scalar<Objectionary>) () -> new OyIndexed(new OyRemote(new ChRemote(tag)))
            )
            .withHelloWorld()
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
}
