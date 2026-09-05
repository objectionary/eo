/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One atom applied to values, as a step of a protocol.
 *
 * <p>It is what a parked record turns into: the λ name of the atom, and
 * the keys of its operands — the receiver first, then the arguments in
 * their positional order. The forma of its value is the one the
 * {@link Op} table binds to the atom, and it nests nothing.</p>
 *
 * @since 0.76.0
 */
public final class Application implements Step {

    /**
     * The name of the step, such as {@code s1}.
     */
    private final String name;

    /**
     * The λ name of the atom, such as {@code L_number_plus}.
     */
    private final String lambda;

    /**
     * The keys of the operands: the receiver first, then the arguments.
     */
    private final List<String> operands;

    /**
     * Ctor.
     * @param label The name of the step, such as {@code s1}
     * @param atom The λ name of the atom, such as {@code L_number_plus}
     * @param keys The keys of the operands, the receiver first
     */
    public Application(final String label, final String atom, final List<String> keys) {
        this.name = label;
        this.lambda = atom;
        this.operands = keys;
    }

    @Override
    public String label() {
        return this.name;
    }

    @Override
    public String atom() {
        return this.lambda;
    }

    @Override
    public String forma() {
        return new Op(this.lambda).forma();
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
