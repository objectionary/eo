/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOdirectory$EOlisted}.
 * @since 0.63
 */
final class EOdirectoryEOlistedTest {

    @Test
    void cannotListPathWithNulCharacter(@TempDir final Path temp) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(String.format("%s%cпапка", temp, (char) 0)));
        final Phi dir = Phi.Φ.take("directory").copy();
        dir.put(0, file);
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                new PhApplication(new EOdirectory$EOlisted(), Phi.RHO, dir)
            ).take(),
            "a path with a NUL character must fail with ExFailure, not a raw InvalidPathException"
        );
    }

    @Test
    void cannotListMissingDirectory(@TempDir final Path temp) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(temp.resolve("отсутствует").toString()));
        final Phi dir = Phi.Φ.take("directory").copy();
        dir.put(0, file);
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                new PhApplication(new EOdirectory$EOlisted(), Phi.RHO, dir)
            ).take(),
            "listing a directory that is not there must fail with ExFailure, not a raw IOException"
        );
    }
}
