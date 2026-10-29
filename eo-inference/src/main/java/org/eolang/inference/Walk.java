/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The per-copy engine (design doc 3 and 4): it reads {@code facts.xml} and
 * says, for every place, which objects it can be when Java runs the
 * program.
 *
 * <p>A copy is one use of a formation, with its own values in its voids,
 * and every place is answered once per copy of the formation around it.
 * The walk is one loop of passes until nothing grows and no stop decides
 * anything (4.1, 4.4), then the end step (4.6), then the telling of each
 * place over the copies that count (5.2).</p>
 *
 * <p>The walk recurses as deep as the chains of reads and calls go, so it
 * runs on a thread of its own with a large stack.</p>
 *
 * @since 0.76.0
 */
public final class Walk {

    /**
     * The stack of the thread the walk runs on, in bytes: eo-runtime needs
     * less than 400 KB, and this leaves room for chains a thousand times
     * deeper.
     */
    private static final long STACK = 1L << 28;

    /**
     * The file {@code facts.xml}.
     */
    private final Path facts;

    /**
     * The names the engine has to know.
     */
    private final Names names;

    /**
     * Ctor, with the names of eo-runtime.
     *
     * @param file The file {@code facts.xml}
     */
    public Walk(final Path file) {
        this(file, new Names());
    }

    /**
     * Ctor.
     *
     * @param file The file {@code facts.xml}
     * @param known The names the engine has to know
     */
    public Walk(final Path file, final Names known) {
        this.facts = file;
        this.names = known;
    }

    /**
     * Walk and tell every place.
     *
     * @return What is told
     * @throws IOException If the facts cannot be read
     */
    Told told() throws IOException {
        final Program program = new FactsXml(this.facts).program();
        final AtomicReference<Told> told = new AtomicReference<>();
        final AtomicReference<Throwable> failure = new AtomicReference<>();
        final Thread thread = new Thread(
            null, () -> told.set(Walk.run(program, this.names)), "eo-inference-walk", Walk.STACK
        );
        thread.setUncaughtExceptionHandler((source, ex) -> failure.set(ex));
        thread.start();
        try {
            thread.join();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IOException(
                String.format("The walk over %s was interrupted", this.facts), ex
            );
        }
        if (told.get() == null) {
            throw new IllegalStateException(
                String.format("The walk over %s failed", this.facts), failure.get()
            );
        }
        return told.get();
    }

    private static Told run(final Program program, final Names names) {
        final long start = System.currentTimeMillis();
        final State state = new State(program, names);
        state.copies().seed();
        state.passes().settle();
        state.end().step();
        final Told told = new Telling(state).told();
        Logger.info(
            Walk.class,
            "The walk settled after %d passes and %d stops in %[ms]s: %d values, %d answers",
            state.passes().size(), state.stops().size(), System.currentTimeMillis() - start,
            state.values().size(), state.memo().size()
        );
        return told;
    }
}
