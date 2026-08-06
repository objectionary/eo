/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;

/**
 * Test case for {@link Archive}.
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class ArchiveTest {

    @Test
    void extractsSafeEntriesInsideDestination(@Mktmp final Path temp) throws Exception {
        final Path jar = temp.resolve("safe.jar");
        ArchiveTest.jar(jar, "org/eolang/ok.eo", "[] > ok");
        final Path dest = temp.resolve("extracted");
        new Archive(jar).extract(dest);
        final Path extracted = dest.resolve("org/eolang/ok.eo");
        MatcherAssert.assertThat(
            "Safe zip entry must land inside the destination directory",
            extracted.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            "Extracted file must contain the original content",
            Files.readString(extracted),
            Matchers.equalTo("[] > ok")
        );
    }

    @Test
    void rejectsZipEntryThatEscapesDestination(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("evil.jar");
        ArchiveTest.jar(jar, "../evil-escaped.txt", "pwned");
        final Path dest = temp.resolve("extracted");
        Assertions.assertThrows(
            IOException.class,
            () -> new Archive(jar).extract(dest),
            "Zip Slip entry must be rejected instead of writing outside the destination"
        );
        MatcherAssert.assertThat(
            "No file may be written for an escaping entry",
            Files.notExists(dest.resolve("../evil-escaped.txt")),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsAbsolutePathEntry(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("absolute.jar");
        ArchiveTest.jar(jar, "/etc/passwd", "pwned");
        Assertions.assertThrows(
            IOException.class,
            () -> new Archive(jar).extract(temp.resolve("extracted")),
            "Absolute-path entry must be rejected"
        );
    }

    @Test
    void rejectsMultiSegmentTraversal(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("deep.jar");
        ArchiveTest.jar(jar, "a/b/../../../evil.txt", "pwned");
        Assertions.assertThrows(
            IOException.class,
            () -> new Archive(jar).extract(temp.resolve("extracted")),
            "Multi-segment traversal must be rejected"
        );
    }

    @Test
    void rejectsMaliciousDirectoryEntry(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("dirtrav.jar");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(jar))) {
            final ZipEntry dir = new ZipEntry("../escape/");
            zip.putNextEntry(dir);
            zip.closeEntry();
        }
        Assertions.assertThrows(
            IOException.class,
            () -> new Archive(jar).extract(temp.resolve("extracted")),
            "Directory entry escaping destination must be rejected"
        );
    }

    @Test
    void extractsEmptyArchive(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("empty.jar");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(jar))) {
            // no entries
        }
        final Path dest = temp.resolve("extracted");
        new Archive(jar).extract(dest);
        MatcherAssert.assertThat(
            "Destination directory must exist after extracting an empty archive",
            dest.toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    void overwritesWhenTwoEntriesResolveToSamePath(@Mktmp final Path temp) throws Exception {
        final Path jar = temp.resolve("collision.jar");
        ArchiveTest.jar(jar, "a.txt", "first", "a.txt", "second");
        final Path dest = temp.resolve("extracted");
        new Archive(jar).extract(dest);
        MatcherAssert.assertThat(
            "Last entry must overwrite the first when they resolve to the same path",
            Files.readString(dest.resolve("a.txt")),
            Matchers.equalTo("second")
        );
    }

    @Test
    void cleansUpAlreadyExtractedEntriesOnRejection(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("partial.jar");
        ArchiveTest.jar(jar, "safe.txt", "ok", "../evil.txt", "pwned");
        final Path dest = temp.resolve("extracted");
        Assertions.assertThrows(
            IOException.class,
            () -> new Archive(jar).extract(dest),
            "Zip Slip must be rejected"
        );
        MatcherAssert.assertThat(
            "Already-extracted safe entry must be cleaned up after rejection",
            Files.notExists(dest.resolve("safe.txt")),
            Matchers.is(true)
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
