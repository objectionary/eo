/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Fingerprint}.
 * @since 0.63
 */
final class FingerprintTest {

    @Test
    void producesTwelveHexCharacters() {
        MatcherAssert.assertThat(
            "the fingerprint must be twelve lowercase hex characters",
            new Fingerprint(FingerprintTest.tojava()).get(),
            Matchers.matchesPattern("[0-9a-f]{12}")
        );
    }

    @Test
    void isDeterministic() {
        MatcherAssert.assertThat(
            "the same resources must always yield the same fingerprint",
            new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get(),
            Matchers.equalTo(
                new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get()
            )
        );
    }

    @Test
    void changesWhenAnyResourceIsAddedOrChanged() {
        MatcherAssert.assertThat(
            "adding a resource to the set must change the fingerprint",
            new Fingerprint(FingerprintTest.tojava()).get(),
            Matchers.not(
                Matchers.equalTo(
                    new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get()
                )
            )
        );
    }

    @Test
    void isSensitiveToResourceOrder() {
        MatcherAssert.assertThat(
            "reordering the resources must change the fingerprint",
            new Fingerprint(FingerprintTest.tojava(), FingerprintTest.classes()).get(),
            Matchers.not(
                Matchers.equalTo(
                    new Fingerprint(FingerprintTest.classes(), FingerprintTest.tojava()).get()
                )
            )
        );
    }

    @Test
    void failsOnMissingResource() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Fingerprint("/org/eolang/maven/transpile/does-not-exist.xsl").get(),
            "a missing resource must fail loudly, not silently produce a wrong fingerprint"
        );
    }

    /**
     * A real bundled transpile XSL, used as a stable input.
     * @return Classpath path of {@code to-java.xsl}
     */
    private static String tojava() {
        return "/org/eolang/maven/transpile/to-java.xsl";
    }

    /**
     * Another real bundled transpile XSL.
     * @return Classpath path of {@code classes.xsl}
     */
    private static String classes() {
        return "/org/eolang/maven/transpile/classes.xsl";
    }
}
