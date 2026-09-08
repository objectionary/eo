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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Concurrency tests for {@link Catalogs}.
 * All tests in that class must be executed in parallel and in order to be sure that
 * everything works fine it's important to run the tests many times.
 * @since 0.29.0
 */
@ExtendWith(MktmpResolver.class)
final class CatalogsTest {

    @RepeatedTest(10)
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

    @Test
    void reusesTheSameFormatForTheSamePath(@Mktmp final Path tmp) {
        final Path file = tmp.resolve("foreign");
        Assertions.assertDoesNotThrow(
            () -> {
                Catalogs.INSTANCE.make(file, "csv");
                Catalogs.INSTANCE.make(file, "csv");
            },
            "Asking for the same path with the same format twice must not fail"
        );
    }

    @Test
    void reusesTheCatalogWhateverTheSpellingOfTheFormat(@Mktmp final Path tmp) {
        final Path file = tmp.resolve("foreign");
        Assertions.assertDoesNotThrow(
            () -> {
                Catalogs.INSTANCE.make(file, " CSV ");
                Catalogs.INSTANCE.make(file, "csv");
            },
            "The same format spelled differently must reuse the same catalog"
        );
    }

    @Test
    void rejectsADisagreeingFormatForAnAlreadyCachedPath(@Mktmp final Path tmp) {
        final Path file = tmp.resolve("foreign");
        Catalogs.INSTANCE.make(file, "csv");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> Catalogs.INSTANCE.make(file, "json"),
            "Asking for the same path with a different format must fail loudly"
        );
    }

    @Test
    void forgetsTheCatalogOfADeletedFile(@Mktmp final Path tmp) throws IOException {
        final Path file = tmp.resolve("foreign.csv");
        final Tojos catalog = Catalogs.INSTANCE.make(file, "csv");
        catalog.add("first");
        catalog.close();
        Files.delete(file);
        Catalogs.INSTANCE.drop(file);
        MatcherAssert.assertThat(
            "the cache is keyed by path alone, so after the file is gone the next catalog must start empty, but it kept the removed rows",
            Catalogs.INSTANCE.make(file, "csv").select(tojo -> true),
            Matchers.empty()
        );
    }

    @Test
    void letsADroppedPathTakeAnotherFormat(@Mktmp final Path tmp) throws IOException {
        final Path file = tmp.resolve("foreign");
        Catalogs.INSTANCE.make(file, "csv").close();
        Catalogs.INSTANCE.drop(file);
        Assertions.assertDoesNotThrow(
            () -> Catalogs.INSTANCE.make(file, "json"),
            "a path nobody holds a catalog for any more must be free to take another format"
        );
    }
}
