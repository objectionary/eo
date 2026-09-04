/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Sha}.
 * @since 0.62.0
 */
@ExtendWith(MktmpResolver.class)
final class ShaTest {

    @Test
    void distinguishesDirsSplitIntoFilesDifferently(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-привет-κόσμε", Long.toHexString(seed));
        new Saved(text.substring(0, 3), temp.resolve("first/a.txt")).value();
        new Saved(text.substring(3), temp.resolve("first/b.txt")).value();
        new Saved(text.substring(0, 7), temp.resolve("second/a.txt")).value();
        new Saved(text.substring(7), temp.resolve("second/b.txt")).value();
        MatcherAssert.assertThat(
            String.format("hashes of differently split dirs are equal, seed=%d", seed),
            new Sha(temp.resolve("first")).toString(),
            Matchers.not(Matchers.equalTo(new Sha(temp.resolve("second")).toString()))
        );
    }

    @Test
    void distinguishesDirsWithRenamedFile(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-日本語", Long.toHexString(seed));
        new Saved(text, temp.resolve("first/alpha.txt")).value();
        new Saved(text, temp.resolve("second/beta.txt")).value();
        MatcherAssert.assertThat(
            String.format("hashes of dirs with a renamed file are equal, seed=%d", seed),
            new Sha(temp.resolve("first")).toString(),
            Matchers.not(Matchers.equalTo(new Sha(temp.resolve("second")).toString()))
        );
    }

    @Test
    void distinguishesDirWithExtraEmptyFile(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-ÆØÅ", Long.toHexString(seed));
        new Saved(text, temp.resolve("first/a.txt")).value();
        new Saved(text, temp.resolve("second/a.txt")).value();
        new Saved("", temp.resolve("second/b.txt")).value();
        MatcherAssert.assertThat(
            String.format("hashes of dirs differing by an empty file are equal, seed=%d", seed),
            new Sha(temp.resolve("first")).toString(),
            Matchers.not(Matchers.equalTo(new Sha(temp.resolve("second")).toString()))
        );
    }

    @Test
    void distinguishesDirsWithFileInAnotherSubdir(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-Ωμέγα", Long.toHexString(seed));
        new Saved(text, temp.resolve("first/one/x.txt")).value();
        new Saved(text, temp.resolve("second/two/x.txt")).value();
        MatcherAssert.assertThat(
            String.format("hashes of dirs with a file in another subdir are equal, seed=%d", seed),
            new Sha(temp.resolve("first")).toString(),
            Matchers.not(Matchers.equalTo(new Sha(temp.resolve("second")).toString()))
        );
    }

    @Test
    void hashesLoneFileWithoutFraming(@Mktmp final Path temp)
        throws IOException, NoSuchAlgorithmException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-ñ-🐘", Long.toHexString(seed));
        final Path file = temp.resolve("lone.txt");
        new Saved(text, file).value();
        MatcherAssert.assertThat(
            String.format("hash of a lone file is not the plain one of its bytes, seed=%d", seed),
            new Sha(file).toString(),
            Matchers.equalTo(
                Base64.getEncoder().encodeToString(
                    MessageDigest.getInstance("SHA-256")
                        .digest(text.getBytes(StandardCharsets.UTF_8))
                )
            )
        );
    }

    @Test
    void ignoresTheFilterForALoneFile(@Mktmp final Path temp)
        throws IOException, NoSuchAlgorithmException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-λόγος", Long.toHexString(seed));
        final Path file = temp.resolve("rejected.txt");
        new Saved(text, file).value();
        MatcherAssert.assertThat(
            String.format(
                "a lone file must hash its own bytes even if the filter rejects it, seed=%d", seed
            ),
            new Sha(file, path -> false).toString(),
            Matchers.equalTo(
                Base64.getEncoder().encodeToString(
                    MessageDigest.getInstance("SHA-256")
                        .digest(text.getBytes(StandardCharsets.UTF_8))
                )
            )
        );
    }

    @Test
    void hashesEqualDirsEqually(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("%s-שלום", Long.toHexString(seed));
        new Saved(text, temp.resolve("first/nested/a.txt")).value();
        new Saved(text, temp.resolve("second/nested/a.txt")).value();
        MatcherAssert.assertThat(
            String.format("hashes of two identical dirs differ, seed=%d", seed),
            new Sha(temp.resolve("first")).toString(),
            Matchers.equalTo(new Sha(temp.resolve("second")).toString())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void hashesADirectoryLinkLikeTheDirectoryItself(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        new Saved(Long.toHexString(seed), temp.resolve("real/a.txt")).value();
        Files.createSymbolicLink(temp.resolve("linked"), temp.resolve("real"));
        MatcherAssert.assertThat(
            String.format(
                "a link to a directory was walked but never entered, so its hash was the one of empty input, seed=%d",
                seed
            ),
            new Sha(temp.resolve("linked")).toString(),
            Matchers.equalTo(new Sha(temp.resolve("real")).toString())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void noticesAChangeBehindADirectoryLink(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        new Saved(Long.toHexString(seed), temp.resolve("real/a.txt")).value();
        Files.createSymbolicLink(temp.resolve("linked"), temp.resolve("real"));
        final String before = new Sha(temp.resolve("linked")).toString();
        new Saved(Long.toHexString(seed + 1L), temp.resolve("real/a.txt")).value();
        MatcherAssert.assertThat(
            String.format(
                "a cache keyed by this hash keeps serving output built from contents that have changed, seed=%d",
                seed
            ),
            new Sha(temp.resolve("linked")).toString(),
            Matchers.not(Matchers.equalTo(before))
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void seesAFileBehindALinkedSubdirectory(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        new Saved(Long.toHexString(seed), temp.resolve("first/a.txt")).value();
        new Saved(Long.toHexString(seed), temp.resolve("second/a.txt")).value();
        new Saved(Long.toHexString(seed), temp.resolve("outside/b.txt")).value();
        Files.createSymbolicLink(temp.resolve("second/sub"), temp.resolve("outside"));
        MatcherAssert.assertThat(
            String.format(
                "a file reachable through a linked subdirectory speaks into the digest, so the two trees must not hash alike, seed=%d",
                seed
            ),
            new Sha(temp.resolve("second")).toString(),
            Matchers.not(Matchers.equalTo(new Sha(temp.resolve("first")).toString()))
        );
    }
}
