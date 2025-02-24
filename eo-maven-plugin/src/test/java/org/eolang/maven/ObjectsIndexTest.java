/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.scalar.ScalarOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link ObjectsIndex}.
 *
 * @since 0.29
 */
final class ObjectsIndexTest {

    @Test
    void contains() throws Exception {
        final AtomicInteger calls = new AtomicInteger(0);
        final String object = "org.eolang.io.stderr";
        final ObjectsIndex index = new ObjectsIndex(
            new ScalarOf<>(
                () -> {
                    calls.incrementAndGet();
                    return Collections.singleton(object);
                }
            )
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            index.contains(object),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            index.contains(object),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            index.contains("unknown"),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            calls.get(),
            Matchers.is(1)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsAndChecksFromRealSource() throws Exception {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new ObjectsIndex().contains("org.eolang.io.stdout"),
            Matchers.is(true)
        );
    }
}
