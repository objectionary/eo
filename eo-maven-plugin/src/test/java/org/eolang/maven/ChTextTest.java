/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link ChText}.
 *
 * @since 0.28.11
 */
@ExtendWith(MktmpResolver.class)
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class ChTextTest {

    /**
     * Test file path in temp dir.
     */
    private Path file;

    @BeforeEach
    void setUp(@Mktmp final Path dir) throws IOException {
        this.file = dir.resolve("tags.txt");
        new Saved(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            this.file
        ).value();
    }

    @ParameterizedTest
    @CsvSource({
        "434868a411b9741fdd4f8a38a5c576e8733345c9, gh-pages",
        "mmmmmmm807fae45ab3ef497451b1066bdd7704c5, master",
        "db0579b48dd717ac015b7af8515f8a56ee6fd521, renovate/actions-checkout-3.x",
        "5fe5ad8d21dbe418038fa4c86e096fb037f290a9, 0.23.15",
        "15c85d7f8cffe15b0deba96e90bdac98a76293bb, 0.23.17",
        "4b19944d86058e3c81e558340a3a13bc335a2b48, 0.23.19",
        "0aa6875c40d099c3f670e93d4134b629566c5643, 0.25.0",
        "ff32e9ff70c2b3be75982757f4b0607dc37b258a, 0.25.5",
        "e0b783692ef749bb184244acb2401f551388a328, 0.26.0",
        "cc554ab82909eebbfdacd8a840f9cf42a99b64cf, 0.27.0",
        "00b60c7b2112cbad4e37ba96b162469a0e75f6df, 0.27.2",
        "6a70071580e95aeac104b2e48293d3dfe0669973, 0.28.0",
        "0c15066a2026cec69d613b709a301f1573f138ec, 0.28.1",
        "9b883935257bd59d1ba36240f7e213d4890df7ca, 0.28.10",
        "54d83d4b1d28075ee623d58fd742eaa529febd3d, 0.28.2",
        "6c6269d1f9a1c81ffe641538f119fe4e12706cb3, 0.28.4",
        "9c9352890b5d30e1b89c9147e7c95a90c9b8709f, 0.28.5",
        "17f89293e5ae6115e9a0234b754b22918c11c602, 0.28.6",
        "5f82cc1edffad67bf4ba816610191403eb18af5d, 0.28.7",
        "be83d9adda4b7c9e670e625fe951c80f3ead4177, 0.28.9"
    })
    void readsCorrectHashByTagFromFile(
        final String hash,
        final String tag
    ) {
        MatcherAssert.assertThat(
            "ChText should read the correct hash by tag from the file, but it didn't",
            new ChText(this.file, tag).value(),
            Matchers.equalTo(hash)
        );
    }

    @Test
    void readsCorrectHashByTagFromSimpleString() {
        MatcherAssert.assertThat(
            "ChText should read the correct hash by tag from a simple string, but it didn't",
            new ChText(
                () -> "434868a411b9741fdd4f8a38a5c576e8733345c9 gh-pages",
                "gh-pages"
            ).value(),
            Matchers.equalTo("434868a411b9741fdd4f8a38a5c576e8733345c9")
        );
    }

    @Test
    void readsHashByNonExistedTag() {
        Assertions.assertThrows(
            ChText.NotFound.class,
            () -> new ChText(
                () -> "434868a411b9741fdd4f8a38a5c576e8733345c9 gh-pages",
                "non-existent-tag"
            ).value(),
            "Must not find this tag, because it does not exist"
        );
    }

    @Test
    void readsHashByExactValueOnly() {
        Assertions.assertThrows(
            ChText.NotFound.class,
            () -> new ChText(
                () -> "434868a411b9741fdd4f8a38a5c576e8733345c9 0.1.22",
                "0.1.2"
            ).value(),
            "Must not find this tag, because it's not an exact match"
        );
    }

    @Test
    void readsHashByHash() {
        MatcherAssert.assertThat(
            "Hash must be itself, even if it's absent in the table",
            new ChText(
                () -> "",
                "434868a411b9741fdd4f8a38a5c576e8733345c9"
            ).value(),
            Matchers.equalTo("434868a411b9741fdd4f8a38a5c576e8733345c9")
        );
    }

    @Test
    void failsAtReachingLimitsOfRetries() {
        Assertions.assertThrows(
            UncheckedIOException.class,
            () ->
                new ChText(
                    () -> {
                        throw new IOException();
                    },
                    "",
                    1
                ).value(),
            "Exception has not been thrown upon reaching the retry limit"
        );
    }

    @Test
    void executesExactlyOnceAtNoError() {
        final AtomicInteger count = new AtomicInteger(0);
        new ChText(
            () -> {
                count.set(1 + count.get());
                return "434868a411b9741fdd4f8a38a5c576e8733345c9 0.1.1";
            },
            "0.1.1",
            3
        ).value();
        MatcherAssert.assertThat(
            "Number of executions is not equal with expected 1 execution",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void executesNotAtTagMatchesHash() {
        final AtomicInteger count = new AtomicInteger(0);
        new ChText(
            () -> {
                count.set(1 + count.get());
                return "";
            },
            "434868a411b9741fdd4f8a38a5c576e8733345c9",
            3
        ).value();
        MatcherAssert.assertThat(
            "Number of executions is not equal with expected 1 execution",
            count.get(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void executesEventually() {
        final AtomicInteger count = new AtomicInteger(0);
        new ChText(
            () -> {
                count.set(1 + count.get());
                if (2 > count.get()) {
                    throw new IOException();
                }
                return "434868a411b9741fdd4f8a38a5c576e8733345c9 0.1.1";
            },
            "0.1.1",
            3
        ).value();
        MatcherAssert.assertThat(
            "Number of executions is not equal with expected 1 execution",
            count.get(),
            Matchers.equalTo(2)
        );
    }
}
