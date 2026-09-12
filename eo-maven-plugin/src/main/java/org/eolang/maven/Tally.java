/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.concurrent.atomic.LongAdder;
import org.eolang.lowering.Rewrite;

/**
 * A lowering pass that adds up, across threads, how many fragments it
 * rewrote, and prints itself as {@code 55 lowered}.
 * @since 0.76.0
 */
final class Tally implements Rewrite {

    /**
     * The pass that rewrites.
     */
    private final Rewrite origin;

    /**
     * The verb saying what the pass did.
     */
    private final String verb;

    /**
     * How many fragments the pass rewrote so far.
     */
    private final LongAdder count;

    /**
     * Ctor.
     * @param pass The pass that rewrites
     * @param name The verb saying what the pass did
     */
    Tally(final Rewrite pass, final String name) {
        this(pass, name, new LongAdder());
    }

    /**
     * Ctor.
     * @param pass The pass that rewrites
     * @param name The verb saying what the pass did
     * @param sum How many fragments the pass rewrote so far
     */
    private Tally(final Rewrite pass, final String name, final LongAdder sum) {
        this.origin = pass;
        this.verb = name;
        this.count = sum;
    }

    @Override
    public String toString() {
        return String.format("%d %s", this.count.sum(), this.verb);
    }

    @Override
    public int rewrite(final Xnav doc) throws IOException {
        final int done = this.origin.rewrite(doc);
        this.count.add(done);
        return done;
    }
}
