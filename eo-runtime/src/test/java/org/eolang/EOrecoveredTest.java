/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOrecovered}.
 *
 * <p>The tests written in `recovered.eo` cover what terminates through an
 * {@link ExFailure}. An extra positional argument terminates through an
 * {@link ExReadOnly} instead, which reaches the recovery wrapped into an
 * {@link ExFailure} only when the program was compiled with
 * {@code trackLocations}, so the check belongs here, where nothing
 * wraps it (#9338).</p>
 *
 * @since 0.64.0
 */
final class EOrecoveredTest {

    @Test
    void recoversFromAnExtraPositionalArgument() {
        final Phi phi = Phi.Φ.take("recovered").copy();
        phi.put(
            0,
            new PhApplication(
                new PhApplication(new PhCached(), 0, new Data.ToPhi(1.0)),
                0,
                new Data.ToPhi(2.0)
            )
        );
        phi.put(1, new Data.ToPhi(7.0));
        MatcherAssert.assertThat(
            "a termination by an extra argument must be recovered, but it wasnt",
            new Dataized(phi).asNumber(),
            Matchers.equalTo(7.0)
        );
    }
}
