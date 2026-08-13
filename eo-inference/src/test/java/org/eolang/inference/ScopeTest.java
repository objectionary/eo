/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Scope}.
 *
 * <p>What the clues understand about a program is checked by the packs of the
 * Maven plugin, which read EO source. This is the resolution underneath, which
 * knows nothing about EO and everything about locators, so it is asked here
 * directly.</p>
 *
 * @since 0.68.0
 */
final class ScopeTest {

    @Test
    void findsParentOfObjectTheReferenceStandsIn() {
        MatcherAssert.assertThat(
            "the parent of the object being formed must be named, but it wasnt",
            new Scope(
                Arrays.asList(
                    "Φ.outer", "Φ.outer.held", "Φ.outer.inner",
                    "Φ.outer.inner.φ", "Φ.outer.inner.φ.ρ"
                ),
                Arrays.asList("Φ.outer", "Φ.outer.held", "Φ.outer.inner")
            ).target("Φ.outer.inner.φ.ρ", "ξ.ρ"),
            Matchers.equalTo("Φ.outer")
        );
    }

    @Test
    void keepsQuietAboutParentOfTopLevelObject() {
        MatcherAssert.assertThat(
            "an object that sits in a package has no formation to name, but one was named",
            new Scope(
                Arrays.asList("Φ.app", "Φ.app.φ", "Φ.app.φ.ρ"),
                Collections.singletonList("Φ.app")
            ).target("Φ.app.φ.ρ", "ξ.ρ"),
            Matchers.emptyString()
        );
    }

    @Test
    void findsNameBoundFurtherOut() {
        MatcherAssert.assertThat(
            "a name bound by a formation above must be found by looking out, but it wasnt",
            new Scope(
                Arrays.asList("Φ.inc", "Φ.inc.x", "Φ.inc.φ", "Φ.inc.φ.ρ"),
                Collections.singletonList("Φ.inc")
            ).target("Φ.inc.φ.ρ", "ξ.x"),
            Matchers.equalTo("Φ.inc.x")
        );
    }

    @Test
    void takesNearestFormationThatBindsName() {
        MatcherAssert.assertThat(
            "the nearest formation that binds the name must win, but a farther one did",
            new Scope(
                Arrays.asList("Φ.a", "Φ.a.x", "Φ.a.b", "Φ.a.b.x", "Φ.a.b.φ"),
                Arrays.asList("Φ.a", "Φ.a.b")
            ).target("Φ.a.b.φ", "ξ.x"),
            Matchers.equalTo("Φ.a.b.x")
        );
    }

    @Test
    void keepsQuietAboutNameNobodyBinds() {
        MatcherAssert.assertThat(
            "a name nobody binds must resolve to nothing, but something was invented",
            new Scope(
                Collections.singletonList("Φ.jar"),
                Collections.singletonList("Φ.jar")
            ).target("Φ.jar.φ", "ξ.nowhere"),
            Matchers.emptyString()
        );
    }

    @Test
    void findsRootObjectOfProgram() {
        MatcherAssert.assertThat(
            "a Φ-rooted name must point at that object, but it didnt",
            new Scope(
                Arrays.asList("Φ.number", "Φ.two", "Φ.two.φ"),
                Arrays.asList("Φ.number", "Φ.two")
            ).target("Φ.two.φ", "Φ.number"),
            Matchers.equalTo("Φ.number")
        );
    }
}
