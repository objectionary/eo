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
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Sha}.
 * @since 0.62
 */
@ExtendWith(MktmpResolver.class)
final class ShaTest {

    @Test
    void doesNotCollideOnDifferentFileBoundaries(@Mktmp final Path temp) throws Exception {
        final Path dira = temp.resolve("A");
        Files.createDirectories(dira);
        Files.writeString(dira.resolve("file1"), "ab");
        Files.writeString(dira.resolve("file2"), "c");
        final Path dirb = temp.resolve("B");
        Files.createDirectories(dirb);
        Files.writeString(dirb.resolve("file1"), "a");
        Files.writeString(dirb.resolve("file2"), "bc");
        MatcherAssert.assertThat(
            "directories with same content concatenation but different file boundaries must not collide",
            new Sha(dira).toString(),
            Matchers.not(Matchers.equalTo(new Sha(dirb).toString()))
        );
    }

    @Test
    void doesNotCollideOnFileRename(@Mktmp final Path temp) throws Exception {
        final Path dira = temp.resolve("A");
        Files.createDirectories(dira);
        Files.writeString(dira.resolve("foo.txt"), "hello");
        final Path dirb = temp.resolve("B");
        Files.createDirectories(dirb);
        Files.writeString(dirb.resolve("bar.txt"), "hello");
        MatcherAssert.assertThat(
            "directories differing only in file name must not collide",
            new Sha(dira).toString(),
            Matchers.not(Matchers.equalTo(new Sha(dirb).toString()))
        );
    }

    @Test
    void providesSameHashForSameDirectory(@Mktmp final Path temp) throws Exception {
        final Path dir = temp.resolve("C");
        Files.createDirectories(dir);
        Files.writeString(dir.resolve("a.txt"), "consistent");
        MatcherAssert.assertThat(
            "same directory must always produce the same hash",
            new Sha(dir).toString(),
            Matchers.equalTo(new Sha(dir).toString())
        );
    }
}