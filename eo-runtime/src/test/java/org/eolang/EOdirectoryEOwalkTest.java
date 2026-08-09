/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for the {@code walk} of {@code directory}.
 * @since 0.63
 */
final class EOdirectoryEOwalkTest {

    @Test
    @Timeout(300)
    @DisabledOnOs(OS.WINDOWS)
    void stopsAtASymbolicLinkToAnAncestor(@TempDir final Path temp) throws IOException {
        final Path sub = Files.createDirectory(temp.resolve("папка"));
        Files.write(sub.resolve("файл.txt"), new byte[0]);
        Files.createSymbolicLink(sub.resolve("ссылка"), temp);
        MatcherAssert.assertThat(
            "walking a directory descends into a symbolic link pointing at its own ancestor",
            this.walked(temp, "**/*.txt"),
            Matchers.contains(sub.resolve("файл.txt").toString())
        );
    }

    @Test
    @Timeout(300)
    @DisabledOnOs(OS.WINDOWS)
    void skipsTheContentsOfASymbolicLinkToADirectory(@TempDir final Path temp) throws IOException {
        final Path sub = Files.createDirectory(temp.resolve("щи"));
        Files.write(sub.resolve("каша.txt"), new byte[0]);
        Files.createSymbolicLink(temp.resolve("ссылка"), sub);
        MatcherAssert.assertThat(
            "walking a directory reaches the entries behind a symbolic link to a directory",
            this.walked(temp, "*/*.txt"),
            Matchers.contains(sub.resolve("каша.txt").toString())
        );
    }

    /**
     * The paths the walk finds in the directory at this path.
     * @param path The path of the directory to walk
     * @param glob The glob the found paths have to match
     * @return The found paths, in the order the walk returns them
     */
    private List<String> walked(final Path path, final String glob) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path.toString()));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        final Phi walk = directory.take("walk").copy();
        walk.put(0, new Data.ToPhi(glob));
        final List<String> found = new ArrayList<>(0);
        for (final Phi item : new TupleToArray(walk).get()) {
            found.add(new Dataized(item).asString());
        }
        return found;
    }
}
