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
                new Sources(temp).with("frame.eo", "[picture] > frame", "  picture > @").program()
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
                new Sources(temp)
                    .with("frame.eo", "[picture] > frame", "  picture > @", "  4 > corners")
                    .program()
            ).decorator("Φ.frame"),
            Matchers.is(false)
        );
    }

    @Test
    void findsVoidObjectPassesOn(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an object whose @ is its own void must pass that void on, but it didnt",
            new Roles(
                new Sources(temp).with("echo.eo", "[sound] > echo", "  sound > @").program()
            ).passes("Φ.echo"),
            Matchers.equalTo("sound")
        );
    }

    @Test
    void findsOwnVoidsAtomGivesBack(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an atom must give back the voids of its own its type names, but it didnt",
            new Roles(
                new Sources(temp)
                    .with("either.eo", "[] > either /A", "  ? > left /A", "  ? > right /A")
                    .program()
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
                new Sources(temp)
                    .with("pot.eo", "[size] > pot", "  [] > lid", "    ^.size > @")
                    .program()
            ).inner("Φ.pot.lid.φ"),
            Matchers.is(true)
        );
    }

    @Test
    void takesNameWithCactusMarkAsCheck(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation named with p🌵 must be a test, but it wasnt",
            new Roles(new Sources(temp).program()).test("Φ.pot.p🌵can-boil"),
            Matchers.is(true)
        );
    }
}
