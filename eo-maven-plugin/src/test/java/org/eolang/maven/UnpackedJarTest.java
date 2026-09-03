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
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link UnpackedJar}.
 * @since 0.64
 */
@ExtendWith(MktmpResolver.class)
final class UnpackedJarTest {

    @Test
    void unpacksAnEntryIntoTheDestination(@Mktmp final Path temp) throws IOException {
        final Path dest = temp.resolve("into");
        UnpackedJarTest.archived(temp.resolve("plain.jar"), "a/b.txt");
        new UnpackedJar(temp.resolve("plain.jar"), dest).unpack();
        MatcherAssert.assertThat(
            "the entry must land under the destination, but it didnt",
            Files.readString(dest.resolve("a/b.txt"), StandardCharsets.UTF_8),
            Matchers.equalTo("hello")
        );
    }

    @Test
    void refusesAnEntryBehindALink(@Mktmp final Path temp) throws IOException {
        final Path dest = temp.resolve("guarded");
        Files.createDirectories(dest);
        Files.createDirectories(temp.resolve("elsewhere"));
        Files.createSymbolicLink(dest.resolve("linked"), temp.resolve("elsewhere"));
        UnpackedJarTest.archived(temp.resolve("sneaky.jar"), "linked/result.txt");
        Assertions.assertThrows(
            IOException.class,
            () -> new UnpackedJar(temp.resolve("sneaky.jar"), dest).unpack(),
            "an entry whose parent is a link out of the destination must be refused"
        );
    }

    @Test
    void writesNothingBehindALink(@Mktmp final Path temp) throws IOException {
        final Path dest = temp.resolve("guarded");
        final Path outside = temp.resolve("elsewhere");
        Files.createDirectories(dest);
        Files.createDirectories(outside);
        Files.createSymbolicLink(dest.resolve("linked"), outside);
        UnpackedJarTest.archived(temp.resolve("sneaky.jar"), "linked/result.txt");
        Assertions.assertThrows(
            IOException.class,
            () -> new UnpackedJar(temp.resolve("sneaky.jar"), dest).unpack(),
            "the entry must be refused before anything is written"
        );
        MatcherAssert.assertThat(
            "the directory the link points at must stay untouched, but it didnt",
            Files.exists(outside.resolve("result.txt")),
            Matchers.is(false)
        );
    }

    private static void archived(final Path jar, final String entry) throws IOException {
        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(jar))) {
            zos.putNextEntry(new ZipEntry(entry));
            zos.write("hello".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
        }
    }
}
