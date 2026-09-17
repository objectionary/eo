/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MjSafe}.
 *
 * @since 0.1
 */
final class MjSafeTest {

    @Test
    void failsBuildWhenUnrollExitErrorIsTrue() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        mojo.unrollExitError = true;
        Assertions.assertThrows(
            MojoFailureException.class,
            mojo::execute,
            "Build must fail when the wrapped Mojo throws and unrollExitError is true"
        );
    }

    @Test
    void failsBuildWhenUnrollExitErrorIsFalse() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        mojo.unrollExitError = false;
        Assertions.assertThrows(
            MojoFailureException.class,
            mojo::execute,
            "Build must still fail when the wrapped Mojo throws, even if unrollExitError is false"
        );
    }

    @Test
    void computesHashFromTagInjectedAfterConstruction() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        final String hex = "abcdefabcdefabcdefabcdefabcdefabcdefabcd";
        mojo.tag = hex;
        Assertions.assertEquals(
            hex.substring(0, 7),
            mojo.hash.value(),
            "hash must be computed from the tag Maven injects after construction, not from the constructor-time default"
        );
    }

    /**
     * The mojo whose exec() always fails.
     *
     * @since 0.1
     */
    @Mojo(name = "failing", defaultPhase = LifecyclePhase.VALIDATE)
    private static final class Failing extends MjSafe {

        @Override
        public void exec() throws IOException {
            throw new IOException("intentional failure for MjSafeTest");
        }
    }
}
