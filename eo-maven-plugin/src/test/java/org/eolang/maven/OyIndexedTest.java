/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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

    /**
     * Object name for stdout.
     */
    private static final String STDOUT_OBJECT = "org.eolang.io.stdout";

    @Test
    void getsFromDelegate() throws Exception {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
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
            CatalogsTest.TO_ADD_MESSAGE,
            new OyIndexed(new Objectionary.Fake()).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }

    @Test
    void containsInFakeIndex() throws IOException {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(() -> Collections.singleton(OyIndexedTest.STDOUT_OBJECT))
            ).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }

    @Test
    void checksContainsInDelegateIfExceptionHappensInIndex() throws IOException {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }
}
