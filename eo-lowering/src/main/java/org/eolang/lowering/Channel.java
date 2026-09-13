/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import javax.json.Json;
import javax.json.JsonObject;

/**
 * The wire between phino and the engine, one JSON object per line.
 *
 * <p>Answers to fires go out as {@code {"id", "𝑛"}}; questions about an
 * operand go out as {@code {"id", "of", "attr", "reduce"}} and block the
 * fire that asked until phino answers with the same id. The ids of the
 * questions come from a counter of their own, far above the ids phino
 * gives to fires, so the two never collide on the line.</p>
 *
 * @since 0.76.0
 */
public final class Channel {

    /**
     * Where the lines go.
     */
    private final Writer out;

    /**
     * The questions waiting for an answer, by id.
     */
    private final Map<Integer, BlockingQueue<String>> open;

    /**
     * The id of the next question.
     */
    private final AtomicInteger next;

    /**
     * Ctor.
     *
     * @param output Where the lines go
     */
    public Channel(final Writer output) {
        this(output, new ConcurrentHashMap<>(0), new AtomicInteger(1_000_000));
    }

    /**
     * Ctor.
     *
     * @param output Where the lines go
     * @param waiting The questions waiting for an answer
     * @param counter The id of the next question
     */
    Channel(final Writer output, final Map<Integer, BlockingQueue<String>> waiting,
        final AtomicInteger counter) {
        this.out = output;
        this.open = waiting;
        this.next = counter;
    }

    /**
     * Answer a fire.
     *
     * @param fire The id of the fire
     * @param phi The φ-expression phino takes as the answer
     * @throws IOException If the line cannot be written
     */
    public void answer(final int fire, final String phi) throws IOException {
        this.said(
            Json.createObjectBuilder().add("id", fire).add("𝑛", phi).build()
        );
    }

    /**
     * Ask phino about an attribute of the formation a fire holds.
     *
     * @param fire The id of the fire
     * @param attr The name of the attribute
     * @param reduce Whether phino must dataize it or hand it back as written
     * @return The φ-expression phino answered with
     * @throws IOException If the line cannot be written
     * @throws InterruptedException If the wait is interrupted
     */
    public String ask(final int fire, final String attr, final boolean reduce)
        throws IOException, InterruptedException {
        final int id = this.next.incrementAndGet();
        final BlockingQueue<String> slot = new LinkedBlockingQueue<>(1);
        this.open.put(id, slot);
        this.said(
            Json.createObjectBuilder()
                .add("id", id)
                .add("of", fire)
                .add("attr", attr)
                .add("reduce", reduce)
                .build()
        );
        return slot.take();
    }

    /**
     * Hand an answer of phino to the fire that asked.
     *
     * @param id The id of the question
     * @param phi The φ-expression phino answered with
     * @throws InterruptedException If the hand-over is interrupted
     */
    public void answered(final int id, final String phi) throws InterruptedException {
        final BlockingQueue<String> slot = this.open.remove(id);
        if (slot == null) {
            throw new IllegalStateException(
                String.format("The answer #%d arrived, but no question with this id is open", id)
            );
        }
        slot.put(phi);
    }

    /**
     * Whether some question is still waiting for its answer.
     *
     * @return True if a fire is blocked on a question
     */
    public boolean waiting() {
        return !this.open.isEmpty();
    }

    /**
     * Write one line.
     *
     * @param message The object
     * @throws IOException If the line cannot be written
     */
    private void said(final JsonObject message) throws IOException {
        synchronized (this.out) {
            this.out.write(message.toString());
            this.out.write('\n');
            this.out.flush();
        }
    }
}
