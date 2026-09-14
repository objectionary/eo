/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One EO method called on values, as a step of a protocol.
 *
 * <p>It takes the name of the step, the method to call, the keys of the
 * receiver and the arguments, and the forma of what comes back, and it
 * answers those four. Where an {@link Application} is a Java operator over
 * locals, this one leaves the EO where it is and calls it, so a fragment
 * leaning on objects nobody lowered is still one atom.</p>
 *
 * @since 0.76.0
 */
final class Dispatch implements Step {

    /**
     * The name of the step, such as {@code s1}.
     */
    private final String name;

    /**
     * The method dispatched, such as {@code minus}.
     */
    private final String method;

    /**
     * The keys of the operands: the receiver first, then the arguments.
     */
    private final List<String> operands;

    /**
     * The forma of the value, {@code object} when the tables witness none.
     */
    private final String carrier;

    /**
     * Ctor.
     *
     * @param label The name of the step, such as {@code s1}
     * @param verb The method dispatched, such as {@code minus}
     * @param keys The keys of the operands, the receiver first
     * @param forma The forma of the value, {@code object} when unwitnessed
     */
    Dispatch(final String label, final String verb,
        final List<String> keys, final String forma) {
        this.name = label;
        this.method = verb;
        this.operands = keys;
        this.carrier = forma;
    }

    @Override
    public String label() {
        return this.name;
    }

    @Override
    public String atom() {
        return String.format(".%s", this.method);
    }

    @Override
    public String forma() {
        return this.carrier;
    }

    @Override
    public List<String> keys() {
        return Collections.unmodifiableList(this.operands);
    }

    @Override
    public List<Protocol> branches() {
        return Collections.emptyList();
    }
}
