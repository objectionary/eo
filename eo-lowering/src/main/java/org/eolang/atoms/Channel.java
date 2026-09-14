/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import javax.json.Json;
import javax.json.JsonObject;

/**
 * The wire between phino and the engine, one JSON object per line.
 *
 * <p>It takes the stream to write on. It sends the answer of a fire, and
 * it asks phino about an operand and answers what comes back, holding the
 * fire that asked until the line with the same id arrives. The ids of
 * questions come from a counter far above the ids phino gives to fires, so
 * the two never collide.</p>
 *
 * @since 0.76.0
 */
final class Channel {

    /**
     * Where the lines go.
     */
    private final Writer out;

    /**
     * The questions waiting for an answer, by id.
     */
    private final Map<Integer, BlockingQueue<Answer>> open;

    /**
     * The id of the next question.
     */
    private final AtomicInteger next;

    /**
     * The lock over the line.
     */
    private final Lock lock;

    /**
     * Ctor.
     *
     * @param output Where the lines go
     */
    Channel(final Writer output) {
        this(
            output, new ConcurrentHashMap<>(0), new AtomicInteger(1_000_000),
            new ReentrantLock()
        );
    }

    /**
     * Ctor.
     *
     * @param output Where the lines go
     * @param waiting The questions waiting for an answer
     * @param counter The id of the next question
     * @param mutex The lock over the line
     */
    Channel(final Writer output, final Map<Integer, BlockingQueue<Answer>> waiting,
        final AtomicInteger counter, final Lock mutex) {
        this.out = output;
        this.open = waiting;
        this.next = counter;
        this.lock = mutex;
    }

    /**
     * Answer a fire.
     *
     * @param fire The id of the fire
     * @param phi The φ-expression phino takes as the answer
     * @throws IOException If the line cannot be written
     */
    void answer(final int fire, final String phi) throws IOException {
        this.said(
            Json.createObjectBuilder().add("id", fire).add("𝑛", phi).build()
        );
    }

    /**
     * Ask phino about an attribute of the formation a fire holds.
     *
     * @param fire The id of the fire
     * @param attr The name of the attribute, or a dotted path down to it
     * @param reduce Whether phino must dataize it or hand it back as written
     * @return The facts phino answered with
     * @throws IOException If the line cannot be written
     * @throws InterruptedException If the wait is interrupted
     */
    Answer ask(final int fire, final String attr, final boolean reduce)
        throws IOException, InterruptedException {
        final int id = this.next.incrementAndGet();
        final BlockingQueue<Answer> slot = new LinkedBlockingQueue<>(1);
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
     * @param answer The facts phino answered with
     * @throws InterruptedException If the hand-over is interrupted
     */
    void answered(final int id, final Answer answer) throws InterruptedException {
        final BlockingQueue<Answer> slot = this.open.remove(id);
        if (slot == null) {
            throw new IllegalStateException(
                String.format("The answer #%d arrived, but no question with this id is open", id)
            );
        }
        slot.put(answer);
    }

    /**
     * Whether some question is still waiting for its answer.
     *
     * @return True if a fire is blocked on a question
     */
    boolean waiting() {
        return !this.open.isEmpty();
    }

    private void said(final JsonObject message) throws IOException {
        this.lock.lock();
        try {
            this.out.write(message.toString());
            this.out.write('\n');
            this.out.flush();
        } finally {
            this.lock.unlock();
        }
    }
}
