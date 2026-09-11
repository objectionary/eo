/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Deleted}.
 * @since 0.52
 */
@ExtendWith(MktmpResolver.class)
final class DeletedTest {

    @Test
    void deletesADirectoryWithItsFiles(@Mktmp final Path temp) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("target/eo"));
        Files.writeString(dir.resolve("stale.xmir"), "<object/>");
        new Deleted(dir.toFile()).get();
        MatcherAssert.assertThat(
            "the directory must be gone with everything in it, but it wasnt",
            Files.exists(dir),
            Matchers.is(false)
        );
    }

    @Test
    void keepsWhatALinkPointsAt(@Mktmp final Path temp) throws IOException {
        final Path outside = Files.createDirectories(temp.resolve("outside"));
        final Path kept = outside.resolve("keep.txt");
        Files.writeString(kept, "not ours to delete");
        final Path dir = Files.createDirectories(temp.resolve("target/eo"));
        final Path link = dir.resolve("link");
        try {
            Files.createSymbolicLink(link, outside);
        } catch (final UnsupportedOperationException | IOException ex) {
            Assumptions.abort("this file system does not make symbolic links");
        }
        new Deleted(dir.toFile()).get();
        MatcherAssert.assertThat(
            "a file behind a link out of the build must survive, but it didnt",
            Files.exists(kept),
            Matchers.is(true)
        );
    }
}
