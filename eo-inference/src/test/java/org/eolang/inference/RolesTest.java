/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Roles}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class RolesTest {

    @Test
    void takesFormationWithOnlyVoidsAndPhiAsDecorator(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a formation with only voids and a @ must be a decorator, but it wasnt",
            new Roles(
                new Sources(temp, "frame.eo", "[picture] > frame\n  picture > @\n").program()
            ).decorator("Φ.frame"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotTakeFormationWithAttributeOfItsOwnAsDecorator(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a formation with an attribute of its own must not be a decorator, but it was",
            new Roles(
                new Sources(
                    temp, "frame.eo", "[picture] > frame\n  picture > @\n  4 > corners\n"
                ).program()
            ).decorator("Φ.frame"),
            Matchers.is(false)
        );
    }

    @Test
    void findsVoidObjectPassesOn(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an object whose @ is its own void must pass that void on, but it didnt",
            new Roles(
                new Sources(temp, "echo.eo", "[sound] > echo\n  sound > @\n").program()
            ).passes("Φ.echo"),
            Matchers.equalTo("sound")
        );
    }

    @Test
    void findsOwnVoidsAtomGivesBack(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an atom must give back the voids of its own its type names, but it didnt",
            new Roles(
                new Sources(
                    temp, "either.eo", "[] > either /A\n  ? > left /A\n  ? > right /A\n"
                ).program()
            ).own("Φ.either"),
            Matchers.contains("left", "right")
        );
    }

    @Test
    void takesReadThroughCaretIntoFormationOfItsOwnAsInner(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a read off ^ into the formation around must be inner, but it wasnt",
            new Roles(
                new Sources(
                    temp, "pot.eo", "[size] > pot\n  [] > lid\n    ^.size > @\n"
                ).program()
            ).inner("Φ.pot.lid.φ"),
            Matchers.is(true)
        );
    }

    @Test
    void takesNameWithTestMarkAsTest(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation named with p🌵 must be a test, but it wasnt",
            new Roles(new Sources(temp).program()).test("Φ.pot.p🌵can-boil"),
            Matchers.is(true)
        );
    }
}
