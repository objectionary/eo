/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.List;

/**
 * A fake phino, answering the questions a fire asks over the channel, in
 * the order they open.
 *
 * @since 0.77.0
 */
final class Oracle implements Runnable {

    /**
     * The channel the fire asks over.
     */
    private final Channel channel;

    /**
     * The answers, one per question.
     */
    private final List<String> replies;

    /**
     * Ctor.
     *
     * @param wire The channel the fire asks over
     * @param answers The answers, one per question
     */
    Oracle(final Channel wire, final String... answers) {
        this(wire, Arrays.asList(answers));
    }

    /**
     * Ctor.
     *
     * @param wire The channel the fire asks over
     * @param answers The answers, one per question
     */
    Oracle(final Channel wire, final List<String> answers) {
        this.channel = wire;
        this.replies = answers;
    }

    @Override
    public void run() {
        try {
            for (int idx = 0; idx < this.replies.size(); ++idx) {
                final long deadline = System.currentTimeMillis() + 5_000L;
                while (!this.channel.waiting() && System.currentTimeMillis() < deadline) {
                    Thread.yield();
                }
                this.channel.answered(1_000_001 + idx, this.replies.get(idx));
            }
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
        }
    }
}
