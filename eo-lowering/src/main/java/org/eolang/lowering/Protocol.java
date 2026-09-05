/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * What a fragment computes, as a program of steps.
 *
 * <p>A reduction settles into this: the steps in their dependency order,
 * the key of the value the fragment answers with — the last step,
 * usually, though a fragment may also collapse into a literal or answer
 * one of its voids unchanged — and the forma of that value. A step is an
 * application or a {@link Fork}, and a fork holds one protocol of this
 * very kind per arm, so a program with choices in it is a tree of
 * protocols whose every path is straight. This is the whole input of
 * code generation: rendering each step as one Java statement, in order,
 * with a block under each arm, is a faithful compilation of the
 * fragment.</p>
 *
 * @since 0.76.0
 */
public final class Protocol {

    /**
     * The steps, in their dependency order.
     */
    private final List<Step> steps;

    /**
     * The key of the value the fragment answers with.
     */
    private final String root;

    /**
     * The forma of that value.
     */
    private final String forma;

    /**
     * Ctor.
     * @param moves The steps, in their dependency order
     * @param answer The key of the value the fragment answers with
     * @param carrier The forma of that value
     */
    public Protocol(final List<Step> moves, final String answer, final String carrier) {
        this.steps = moves;
        this.root = answer;
        this.forma = carrier;
    }

    /**
     * The steps.
     * @return The steps, in their dependency order
     */
    public List<Step> moves() {
        return Collections.unmodifiableList(this.steps);
    }

    /**
     * The key of the value the fragment answers with.
     * @return A key such as {@code sym:s2} or {@code number:40-14-...}
     */
    public String answer() {
        return this.root;
    }

    /**
     * The forma of the value.
     * @return One of {@code number}, {@code bool}, {@code bytes}
     */
    public String carrier() {
        return this.forma;
    }
}
