/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Snapshot}.
 *
 * @since 0.1
 */
final class SnapshotTest {

    @Test
    void isEmptyWhenGivenNull() {
        MatcherAssert.assertThat(
            "Snapshot of null must be empty, but it wasn't",
            new Snapshot(null).empty(),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresMutationOfTheSourceArray() {
        final byte[] raw = {(byte) 0x01};
        final Snapshot snapshot = new Snapshot(raw);
        raw[0] = (byte) 0x02;
        MatcherAssert.assertThat(
            "Snapshot must copy the source array, but it didn't",
            snapshot.bytes(),
            Matchers.equalTo(new byte[] {(byte) 0x01})
        );
    }

    @Test
    void ignoresMutationOfAPreviouslyReturnedArray() {
        final Snapshot snapshot = new Snapshot(new byte[] {(byte) 0x01});
        snapshot.bytes()[0] = (byte) 0x03;
        MatcherAssert.assertThat(
            "Snapshot must return a fresh copy every time, but it didn't",
            snapshot.bytes(),
            Matchers.equalTo(new byte[] {(byte) 0x01})
        );
    }

    @Test
    void keepsPhDefaultImmuneToMutationOfTheSourceArray() {
        final byte[] raw = {(byte) 0x01};
        final Phi phi = new PhDefault(raw);
        raw[0] = (byte) 0x02;
        MatcherAssert.assertThat(
            "PhDefault must copy the source array via Snapshot, but it didn't",
            phi.delta(),
            Matchers.equalTo(new byte[] {(byte) 0x01})
        );
    }

    @Test
    void keepsPhDefaultImmuneToMutationOfAPreviouslyReturnedDelta() {
        final Phi phi = new PhDefault(new byte[] {(byte) 0x01});
        phi.delta()[0] = (byte) 0x03;
        MatcherAssert.assertThat(
            "PhDefault must return a fresh array via Snapshot on every #delta() call, but it didn't",
            phi.delta(),
            Matchers.equalTo(new byte[] {(byte) 0x01})
        );
    }
}
