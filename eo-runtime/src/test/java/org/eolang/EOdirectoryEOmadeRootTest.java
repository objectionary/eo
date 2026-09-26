/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@code directory.made} at a path that is its own parent.
 *
 * <p>{@code made} walks up the tree one {@code dirname} at a time and stops at
 * the first level that is already there. A drive root with no drive behind it
 * is not there and hands back itself as its own parent, so the walk asked for
 * the same level forever and the heap ran out (#7480).</p>
 *
 * <p>The case cannot be written as a {@code ++>} test in {@code made.eo},
 * because it is not portable: {@code made} reaches for {@code Q.path}, which
 * is the path of the host it runs on, and on POSIX every fixed point of
 * {@code dirname} is a directory that exists ({@code /} and {@code .}), so the
 * walk ends there for the ordinary reason and the guard is never the thing
 * under test. Only a drive letter and a UNC share root are fixed points that
 * are absent, and only Windows has them.</p>
 *
 * @since 0.78.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOdirectoryEOmadeRootTest {

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void refusesADriveRootWithNoDriveBehindIt() {
        MatcherAssert.assertThat(
            "a drive root that is its own parent must end the walk with the reason, but it didnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> this.make("Q:\\"),
                "a drive root with no drive behind it was expected to be refused"
            ).getMessage(),
            Matchers.containsString("it has no parent")
        );
    }

    private void make(final String path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        new Dataized(directory.take("made")).take();
    }
}
