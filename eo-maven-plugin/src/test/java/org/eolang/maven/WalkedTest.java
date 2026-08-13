/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link Walked}.
 * @since 0.62.0
 */
final class WalkedTest {

    @Test
    void readsBackTheTreeTheParseGoalWrote(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <o name=\"объект%d\">%n    <o base=\"Φ.foo.bar\" bare=\"\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format("the tree read back is not the one on disk, seed %d", seed),
            new Walked(file).tree().xpath("//o[@bare]/@base").get(0),
            Matchers.equalTo("Φ.foo.bar")
        );
    }

    @Test
    void dropsTheBlanksTheLayoutLeftBetweenElements(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <o name=\"объект%d\">%n    <o base=\"Φ.bar\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format("the indentation was carried into the tree, seed %d", seed),
            new Walked(file).tree().nodes("//text()[not(normalize-space())]"),
            Matchers.empty()
        );
    }
}
