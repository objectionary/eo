/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for the {@code is-symlink} attribute of {@code file} in
 * {@code file.eo}, which no Java atom stands behind.
 * @since 0.75.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOfileEOissymlinkTest {

    @Test
    void tellsALinkToADirectoryFromTheDirectory(@TempDir final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a symbolic link to a directory is taken for the directory it points at",
            this.linking(
                Files.createSymbolicLink(
                    temp.resolve("ссылка"),
                    Files.createDirectory(temp.resolve("папка"))
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void tellsADirectoryFromALinkToIt(@TempDir final Path temp) throws IOException {
        Files.createSymbolicLink(
            temp.resolve("ссылка"),
            Files.createDirectory(temp.resolve("папка"))
        );
        MatcherAssert.assertThat(
            "a directory with a symbolic link beside it is taken for a link",
            this.linking(temp.resolve("папка")),
            Matchers.is(false)
        );
    }

    /**
     * The answer the file at this path gives about being a symbolic link.
     * @param path The path of the file to ask
     * @return True when the path is a symbolic link of its own
     */
    private boolean linking(final Path path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path.toString()));
        final Phi asking = file.take("is-symlink").copy();
        asking.put(0, new Data.ToPhi(false));
        return new Dataized(asking).asBool();
    }
}
