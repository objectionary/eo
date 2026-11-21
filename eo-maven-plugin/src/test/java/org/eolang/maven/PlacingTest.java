/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import org.cactoos.list.ListOf;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test cases for {@link Placing}.
 *
 * <p>The test logging configuration turns {@code org.eolang} off, and the
 * cases run alone, so that no Maven goal running in parallel turns debug
 * logging on for them.</p>
 *
 * @since 0.76.0
 */
@Isolated
final class PlacingTest {

    @Test
    void walksEachDependencyOnceWithoutDebugLogging(@TempDir final Path temp)
        throws IOException {
        final AtomicInteger walks = new AtomicInteger(0);
        new Placing(
            new TjsPlaced(temp.resolve("placed.json")),
            PlacingTest.home(temp),
            temp.resolve("classes"),
            new SetOf<>("**"),
            new SetOf<>(),
            false,
            dir -> {
                walks.incrementAndGet();
                return new WkDefault(dir);
            }
        ).exec();
        MatcherAssert.assertThat(
            "Without debug logging, each of two dependencies must be walked once, for placing only",
            walks.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void selectsCatalogOncePerDependencyWithoutDebugLogging(@TempDir final Path temp)
        throws IOException {
        final AtomicInteger selections = new AtomicInteger(0);
        final Tojos origin = Catalogs.INSTANCE.make(temp.resolve("placed.json"), "json");
        new Placing(
            new TjsPlaced(
                () -> new Tojos() {
                    @Override
                    public Tojo add(final String name) {
                        return origin.add(name);
                    }

                    @Override
                    public List<Tojo> select(final Predicate<Tojo> filter) {
                        selections.incrementAndGet();
                        return origin.select(filter);
                    }

                    @Override
                    public void close() throws IOException {
                        origin.close();
                    }
                }
            ),
            PlacingTest.home(temp),
            temp.resolve("classes"),
            new SetOf<>("**"),
            new SetOf<>(),
            false
        ).exec();
        MatcherAssert.assertThat(
            "Without debug logging, the catalog must be selected per dependency, never per binary",
            selections.get(),
            Matchers.equalTo(2)
        );
    }

    private static Path home(final Path temp) throws IOException {
        final Path home = temp.resolve("home");
        int total = 0;
        for (final String dep : new ListOf<>("org.foo/first/-/0.1", "org.foo/second/-/0.2")) {
            for (int idx = 0; idx < 3; ++idx) {
                final Path file = home.resolve(dep).resolve(String.format("Bar%d.class", total));
                Files.createDirectories(file.getParent());
                Files.write(file, "bytes".getBytes(StandardCharsets.UTF_8));
                total += 1;
            }
        }
        return home;
    }
}
