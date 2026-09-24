/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys;

import java.security.SecureRandom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.EOposix$EOread;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Intake}.
 *
 * @since 0.77.0
 */
final class IntakeTest {

    @Test
    void handsBackOnlyTheBytesTheCallFilled() {
        final int size = new SecureRandom().nextInt(512) + 64;
        MatcherAssert.assertThat(
            "the data must not carry the part of the buffer the call left untouched",
            new Dataized(
                new Intake(
                    new PhApplication(new EOposix$EOread(), "size", new Data.ToPhi(size)),
                    Phi.Φ.take("posix").take("read-return"),
                    (buffer, wanted) -> 13
                ).it().take("data")
            ).take().length,
            Matchers.equalTo(13)
        );
    }

    @Test
    void handsBackNoBytesWhenTheCallFails() {
        MatcherAssert.assertThat(
            "a failed call must not hand back a buffer it never filled",
            new Dataized(
                new Intake(
                    new PhApplication(new EOposix$EOread(), "size", new Data.ToPhi(41)),
                    Phi.Φ.take("posix").take("read-return"),
                    (buffer, wanted) -> -1
                ).it().take("data")
            ).take().length,
            Matchers.equalTo(0)
        );
    }

    @ParameterizedTest
    @ValueSource(doubles = {-7.0, 1.5, Double.POSITIVE_INFINITY, Integer.MAX_VALUE})
    void refusesSizeNoBufferCanHave(final double size) {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Intake(
                new PhApplication(new EOposix$EOread(), "size", new Data.ToPhi(size)),
                Phi.Φ.take("posix").take("read-return"),
                (buffer, wanted) -> 0
            ).it(),
            String.format("a size of %s was expected to be refused before the call is made", size)
        );
    }
}
