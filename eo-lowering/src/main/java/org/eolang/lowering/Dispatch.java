/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One method dispatched back into EO, as a step of a protocol.
 *
 * <p>It is what a parked dispatch marker turns into: the method, the
 * keys of its operands — the receiver first, then the arguments in
 * their positional order — and the forma of its value. Unlike an
 * {@link Application}, which is a Java operator over locals, this step
 * is a call, spelled the way a hand-written atom spells one: the
 * receiver and the arguments are wrapped back into objects, the method
 * is taken of the receiver and applied to the arguments, and the value
 * is dataized into the forma the tables of {@code eo:inference} witness
 * for it, or held as the object it is when they witness none. The EO
 * behind the method stays where it is, so a fragment leaning on
 * objects the universe does not model is still one atom, one class,
 * one body. The step is strict: every argument is a value computed
 * before it, the way every step of a protocol is, and the arms of an
 * {@code if} stay the one thing a protocol leaves lazy.</p>
 *
 * @since 0.76.0
 */
public final class Dispatch implements Step {

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
     * @param label The name of the step, such as {@code s1}
     * @param verb The method dispatched, such as {@code minus}
     * @param keys The keys of the operands, the receiver first
     * @param forma The forma of the value, {@code object} when unwitnessed
     */
    public Dispatch(final String label, final String verb,
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
