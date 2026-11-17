/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Copies}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class CopiesTest {

    @Test
    void listsTheCopiesInTheOrderOfTheirNames(@Mktmp final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp.resolve("1-planting"));
        Files.write(home.resolve("zeta.xmir"), new byte[0]);
        Files.write(home.resolve("alpha.xmir"), new byte[0]);
        MatcherAssert.assertThat(
            "the copies must be listed in the order of their names, but they arent",
            new Copies(temp),
            Matchers.contains(home.resolve("alpha.xmir"), home.resolve("zeta.xmir"))
        );
    }

    @Test
    void listsTheCopiesOfAnEarlierPlanting(@Mktmp final Path temp) throws IOException {
        final Path planting = Files.createDirectories(temp.resolve("5-planting"));
        Files.write(planting.resolve("kept.xmir"), new byte[0]);
        MatcherAssert.assertThat(
            "the copies must be listed from the planting of an earlier build, but they arent",
            new Copies(temp),
            Matchers.contains(planting.resolve("kept.xmir"))
        );
    }

    @Test
    void failsNamingTheHomeWithoutPlanting(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the home with no planting, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Copies(temp).iterator(),
                "a home without planting must fail the listing of the copies"
            ).getMessage(),
            Matchers.containsString(temp.toString())
        );
    }
}
