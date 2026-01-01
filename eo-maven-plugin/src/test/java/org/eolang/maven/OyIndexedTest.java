/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.util.Collections;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link OyIndexed}.
 *
 * @since 0.29
 */
final class OyIndexedTest {

    @Test
    void getsFromDelegate() throws Exception {
        MatcherAssert.assertThat(
            "OyIndexed must get a line of program, but it doesn't",
            new TextOf(new OyIndexed(new Objectionary.Fake()).get("foo")).asString(),
            Matchers.equalTo(
                "# No comments.\n[] > sprintf\n"
            )
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void containsInRealIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must contain stdout object, but it doesn't",
            new OyIndexed(new Objectionary.Fake()).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void containsInFakeIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with fake index must contain stdout object, but it doesn't",
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(() -> Collections.singleton(this.stdout()))
            ).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void checksContainsInDelegateIfExceptionHappensInIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with an exception must contain stdout object, but it doesn't",
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksIsDirectoryForObject() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must contain stdout object, but it doesn't",
            new OyIndexed(new Objectionary.Fake()).isDirectory(this.stdout()),
            Matchers.is(false)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksIsDirectoryForDirectory() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must not contain directory, but it does",
            new OyIndexed(new Objectionary.Fake()).isDirectory("xxx"),
            Matchers.is(false)
        );
    }

    /**
     * Returns the stdout path.
     */
    private String stdout() {
        return "org.eolang.io.stdout";
    }
}
