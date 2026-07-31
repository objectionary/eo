/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

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
                    new PhApplication(new EOdirectory$EOwalk(), Phi.RHO, dir),
                    "glob", new Data.ToPhi("[")
                )
            ).take(),
            "a malformed glob must fail with ExFailure, not a raw PatternSyntaxException"
        );
    }
}
