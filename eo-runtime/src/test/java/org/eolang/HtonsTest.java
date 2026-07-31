/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for the {@code socket.htons} attribute.
 * @since 0.40
 */
final class HtonsTest {

    @Test
    void convertsPortAboveSignedShortLimitToNetworkByteOrder() {
        final Phi htons = Phi.Φ.take("socket").take("htons").copy();
        htons.put(0, new Data.ToPhi(40_000));
        MatcherAssert.assertThat(
            "htons should convert a port above 32767 to its network byte order bytes, but it didn't",
            new Dataized(htons).take(),
            Matchers.equalTo(new byte[]{(byte) 0x9C, (byte) 0x40})
        );
    }

    @Test
    void rejectsPortOutsideValidRange() {
        final Phi htons = Phi.Φ.take("socket").take("htons").copy();
        htons.put(0, new Data.ToPhi(70_000));
        Assertions.assertThrows(
            ExFailure.class,
            new Dataized(htons)::take,
            "htons should fail for a port above 65535, but it didn't"
        );
    }
}
