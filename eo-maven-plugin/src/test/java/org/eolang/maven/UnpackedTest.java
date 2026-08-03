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
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Unpacked}.
 * @since 0.62.0
 */
@ExtendWith(MktmpResolver.class)
final class UnpackedTest {

    @Test
    void unpacksSafeEntriesInsideDestination(@Mktmp final Path temp) throws Exception {
        final Path jar = temp.resolve("safe.jar");
        UnpackedTest.jar(jar, "org/eolang/ok.eo", "[] > ok");
        final Path dest = temp.resolve("unpacked");
        new Unpacked(jar, dest).unpack();
        MatcherAssert.assertThat(
            "Safe zip entry must land inside the destination directory",
            dest.resolve("org/eolang/ok.eo").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void rejectsZipEntryThatEscapesDestination(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("evil.jar");
        UnpackedTest.jar(jar, "../evil-escaped.txt", "pwned");
        Assertions.assertThrows(
            IOException.class,
            () -> new Unpacked(jar, temp.resolve("unpacked")).unpack(),
            "Zip Slip entry must be rejected instead of writing outside the destination"
        );
    }

    /**
     * Writes a jar with alternating entry-name / content pairs.
     * @param jar Destination jar path
     * @param entries Alternating names and payloads
     * @throws IOException If writing fails
     */
    private static void jar(final Path jar, final String... entries) throws IOException {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(jar))) {
            for (int idx = 0; idx < entries.length; idx += 2) {
                zip.putNextEntry(new ZipEntry(entries[idx]));
                zip.write(entries[idx + 1].getBytes(StandardCharsets.UTF_8));
                zip.closeEntry();
            }
        }
    }
}