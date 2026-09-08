/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Scanned}.
 * @since 0.73.4
 */
@ExtendWith(MktmpResolver.class)
final class ScannedTest {

    @Test
    void skipsDirectoryOwnedByVersionControl(@Mktmp final Path temp) throws Exception {
        new Saved("[] > привет", temp.resolve("привет.eo")).value();
        new Saved("", temp.resolve(".git/objects/pack/каталог.pack")).value();
        MatcherAssert.assertThat(
            "a file inside .git must be pruned by the scan, but it was walked",
            new Scanned(temp),
            Matchers.contains(temp.resolve("привет.eo"))
        );
    }

    @Test
    void findsNothingInMissingDirectory(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a directory that is not there holds no files, but some were found",
            new Scanned(temp.resolve("absent")),
            Matchers.emptyIterable()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void skipsDirectoryReachedThroughALink(@Mktmp final Path temp) throws Exception {
        new Saved("[] > omega", temp.resolve("sources/ωmega.eo")).value();
        Files.createSymbolicLink(temp.resolve("mirror"), temp.resolve("sources"));
        MatcherAssert.assertThat(
            "a linked directory must not be descended into, since its files carry their own names",
            new Scanned(temp),
            Matchers.contains(temp.resolve("sources/ωmega.eo"))
        );
    }
}
