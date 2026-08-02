/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;

/**
 * Assertion that a server thread received exactly the bytes a client sent,
 * shared by socket syscall tests.
 * @since 0.40.0
 */
public final class ReceivedBytes {

    /**
     * Bytes the client sent.
     */
    private final byte[] sent;

    /**
     * Number of bytes the server reported as received.
     */
    private final AtomicInteger count;

    /**
     * Bytes the server received.
     */
    private final AtomicReference<byte[]> received;

    /**
     * Ctor.
     * @param sent Bytes the client sent
     * @param count Number of bytes the server reported as received
     * @param received Bytes the server received
     */
    public ReceivedBytes(
        final byte[] sent, final AtomicInteger count, final AtomicReference<byte[]> received
    ) {
        this.sent = sent.clone();
        this.count = count;
        this.received = received;
    }

    /**
     * Verify the server received exactly what the client sent.
     */
    public void verify() {
        MatcherAssert.assertThat(
            "Server had to receive the message from the client, but it didn't",
            this.count.get(),
            Matchers.equalTo(this.sent.length)
        );
        MatcherAssert.assertThat(
            "Received bytes must be equal to sent, but they didn't",
            new String(this.received.get(), StandardCharsets.UTF_8),
            Matchers.equalTo(new String(this.sent, StandardCharsets.UTF_8))
        );
    }
}
