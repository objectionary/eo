/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A port to bind to, and a latch signaling the server has started
 * listening on it, shared between a test method and its background
 * server thread in socket syscall tests.
 * @since 0.40.0
 */
public final class ServerHandoff {

    /**
     * Port to bind to, updated if a bind attempt picks a taken candidate.
     */
    private final AtomicInteger candidate;

    /**
     * Counted down once the server thread's listen() succeeds.
     */
    private final CountDownLatch listening;

    /**
     * Ctor.
     * @param port First port candidate to try
     */
    public ServerHandoff(final int port) {
        this.candidate = new AtomicInteger(port);
        this.listening = new CountDownLatch(1);
    }

    /**
     * The port candidate, mutable so a bind retry can pick a new one.
     * @return Port candidate
     */
    public AtomicInteger port() {
        return this.candidate;
    }

    /**
     * Signal that the server has started listening.
     */
    public void ready() {
        this.listening.countDown();
    }

    /**
     * Wait for the server to signal it has started listening.
     * @param millis Timeout in milliseconds
     * @return True if the server signaled in time
     * @throws InterruptedException If interrupted while waiting
     */
    public boolean awaited(final long millis) throws InterruptedException {
        return this.listening.await(millis, TimeUnit.MILLISECONDS);
    }
}
