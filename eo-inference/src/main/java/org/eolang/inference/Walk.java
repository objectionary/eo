/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

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
        final FutureTask<Told> told = new FutureTask<>(() -> this.run(program));
        new Thread(null, told, "eo-inference-walk", Walk.STACK).start();
        try {
            return told.get();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IOException(
                String.format("The walk over %s was interrupted", this.facts), ex
            );
        } catch (final ExecutionException ex) {
            throw new IllegalStateException(
                String.format("The walk over %s failed", this.facts), ex
            );
        }
    }

    private Told run(final Program program) {
        final long start = System.currentTimeMillis();
        final World world = new World(program, this.names);
        final State state = new State(world);
        state.seed();
        state.settle();
        state.step();
        final Told told = new Telling(state, world).told();
        Logger.info(
            this,
            "The walk settled after %d passes and %d stops in %[ms]s: %d values, %d answers",
            state.passes(), state.stops(), System.currentTimeMillis() - start,
            world.size(), state.answers()
        );
        return told;
    }
}
