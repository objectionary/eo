/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.Together;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.nio.file.Path;
import java.util.UUID;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Concurrency tests for {@link Catalogs}.
 * All tests in that class must be executed in parallel and in order to be sure that
 * everything works fine it's important to run the tests many times.
 * @since 0.29.0
 */
@ExtendWith(MktmpResolver.class)
@SuppressWarnings("PMD.JUnit5TestShouldBePackagePrivate")
public final class CatalogsTest {
    @Test
    void readsFromTojosConcurrently(@Mktmp final Path tmp) {
        final Tojos tojos = Catalogs.INSTANCE.make(tmp.resolve("foreign"), "json");
        MatcherAssert.assertThat(
            "adds different elements to catalog",
            new Together<>(
                thread -> {
                    final Tojo tojo = tojos.add(UUID.randomUUID().toString());
                    final String key = "foo";
                    tojo.set(key, UUID.randomUUID().toString());
                    tojo.get(key);
                    tojo.get(key);
                    tojo.get(key);
                    tojo.get(key);
                    return true;
                }
            ),
            Matchers.not(Matchers.hasItem(false))
        );
    }
}
