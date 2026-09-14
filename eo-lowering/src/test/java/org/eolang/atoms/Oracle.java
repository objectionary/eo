/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import javax.json.Json;
import javax.json.JsonReader;

/**
 * A fake phino, answering the questions a fire asks over the channel, in
 * the order they open, each answer a JSON object of the facts phino would
 * spell, such as {@code {"Δ":"2A-"}} or {@code {"λ":"S7"}}.
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
                    Thread.sleep(10L);
                }
                try (
                    JsonReader reader = Json.createReader(
                        new StringReader(this.replies.get(idx))
                    )
                ) {
                    this.channel.answered(1_000_001 + idx, new Answer(reader.readObject()));
                }
            }
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
        }
    }
}
