/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * What one partial dataization did.
 *
 * <p>Under {@code --partial} a run always ends successfully: either
 * everything dataized and the output is data, or some atoms parked and
 * the output is the residual expression. The records tell the story
 * either way — every fired atom with its result, every parked atom with
 * its input — and the reduction loop reads only them, never the residual
 * text, since the sites it rewrites live in its own tree.</p>
 *
 * @since 0.76.0
 */
public final class Trace {

    /**
     * Whether the run dataized everything.
     */
    private final boolean whole;

    /**
     * The evaluation records, in the order phino wrote them.
     */
    private final List<Evaluation> lines;

    /**
     * Ctor.
     * @param total Whether the run dataized everything
     * @param records The evaluation records, in the order phino wrote them
     */
    public Trace(final boolean total, final List<Evaluation> records) {
        this.whole = total;
        this.lines = records;
    }

    /**
     * Whether the run dataized everything.
     * @return True if the output was data, not a residual expression
     */
    public boolean total() {
        return this.whole;
    }

    /**
     * The evaluation records.
     * @return The records, in the order phino wrote them
     */
    public List<Evaluation> records() {
        return Collections.unmodifiableList(this.lines);
    }
}
