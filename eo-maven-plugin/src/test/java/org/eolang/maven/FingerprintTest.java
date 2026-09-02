/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Fingerprint}.
 * @since 0.63
 */
@ExtendWith(MktmpResolver.class)
final class FingerprintTest {

    @Test
    void producesTwelveHexCharacters() {
        MatcherAssert.assertThat(
            "the fingerprint must be twelve lowercase hex characters",
            new Fingerprint(FingerprintTest.tojava()).get(),
            Matchers.matchesPattern("[0-9a-f]{12}")
        );
    }

    @Test
    void isDeterministic() {
        MatcherAssert.assertThat(
            "the same resources must always yield the same fingerprint",
            new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get(),
            Matchers.equalTo(
                new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get()
            )
        );
    }

    @Test
    void changesWhenAnyResourceIsAddedOrChanged() {
        MatcherAssert.assertThat(
            "adding a resource to the set must change the fingerprint",
            new Fingerprint(FingerprintTest.tojava()).get(),
            Matchers.not(
                Matchers.equalTo(
                    new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get()
                )
            )
        );
    }

    @Test
    void isSensitiveToResourceOrder() {
        MatcherAssert.assertThat(
            "reordering the resources must change the fingerprint",
            new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get(),
            Matchers.not(
                Matchers.equalTo(
                    new Fingerprint(FingerprintTest.classes(), FingerprintTest.tojava()).get()
                )
            )
        );
    }

    @Test
    void failsOnMissingResource() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Fingerprint("/org/eolang/maven/transpile/does-not-exist.xsl").get(),
            "a missing resource must fail loudly, not silently produce a wrong fingerprint"
        );
    }

    @Test
    void distinguishesDirsSplitIntoFilesDifferently(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-привет-κόσμε", Long.toHexString(seed));
        new Saved(text.substring(0, 3), temp.resolve("first/a.txt")).value();
        new Saved(text.substring(3), temp.resolve("first/b.txt")).value();
        new Saved(text.substring(0, 7), temp.resolve("second/a.txt")).value();
        new Saved(text.substring(7), temp.resolve("second/b.txt")).value();
        MatcherAssert.assertThat(
            String.format("fingerprints of differently split dirs are equal, seed=%d", seed),
            new Fingerprint(temp.resolve("first")).get(),
            Matchers.not(
                Matchers.equalTo(new Fingerprint(temp.resolve("second")).get())
            )
        );
    }

    @Test
    void distinguishesDirsWithRenamedFile(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-日本語", Long.toHexString(seed));
        new Saved(text, temp.resolve("first/alpha.txt")).value();
        new Saved(text, temp.resolve("second/beta.txt")).value();
        MatcherAssert.assertThat(
            String.format("fingerprints of dirs with a renamed file are equal, seed=%d", seed),
            new Fingerprint(temp.resolve("first")).get(),
            Matchers.not(
                Matchers.equalTo(new Fingerprint(temp.resolve("second")).get())
            )
        );
    }

    @Test
    void distinguishesDirWhereNameEatsTheContent(@Mktmp final Path temp) throws IOException {
        new Saved("bc", temp.resolve("first/a")).value();
        new Saved("c", temp.resolve("second/ab")).value();
        MatcherAssert.assertThat(
            "a file name and its content blend into one stream, but they must not",
            new Fingerprint(temp.resolve("first")).get(),
            Matchers.not(
                Matchers.equalTo(new Fingerprint(temp.resolve("second")).get())
            )
        );
    }

    private static String tojava() {
        return "/org/eolang/maven/transpile/to-java.xsl";
    }

    private static String classes() {
        return "/org/eolang/maven/transpile/classes.xsl";
    }
}
