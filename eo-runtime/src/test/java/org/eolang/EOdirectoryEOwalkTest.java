/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.file.InvalidPathException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOdirectory$EOwalk}.
 * @since 0.63
 */
final class EOdirectoryEOwalkTest {

    @Test
    void wrapsMalformedGlobIntoExFailure() {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi("/tmp"));
        final Phi dir = Phi.Φ.take("directory").copy();
        dir.put(0, file);
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                new PhApplication(
                    new PhApplication(new EOdirectory$EOwalk(new Silent()), Phi.RHO, dir),
                    "glob", new Data.ToPhi("[")
                )
            ).take(),
            "a malformed glob must fail with ExFailure, not a raw PatternSyntaxException"
        );
    }

    @Test
    void wrapsInvalidPathIntoExFailure() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(this.invalidPathWalk()).take(),
            "a path with a NUL character must fail with ExFailure, not a raw InvalidPathException"
        );
    }

    @Test
    void includesInvalidPathReason() {
        final ExFailure failure = this.failure(
            this.invalidPathWalk()
        );
        MatcherAssert.assertThat(
            "failure must identify why parsing failed",
            failure.getMessage(),
            Matchers.containsString(
                ((InvalidPathException) failure.getCause()).getReason()
            )
        );
    }

    /**
     * Make a directory walk application with an invalid path.
     * @return Directory walk application
     */
    private Phi invalidPathWalk() {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(String.format("/tmp/%cinvalid", (char) 0)));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        return new PhApplication(
            new PhApplication(new EOdirectory$EOwalk(new Silent()), Phi.RHO, directory),
            "glob", new Data.ToPhi("*")
        );
    }

    /**
     * Get a failure from the directory walk application.
     * @param walk Directory walk application
     * @return Failure raised by the atom
     */
    private ExFailure failure(final Phi walk) {
        try {
            new Dataized(walk).take();
        } catch (final ExFailure err) {
            return err;
        }
        throw new IllegalStateException("Invalid path must fail");
    }
}
