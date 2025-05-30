/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Function;

/**
 * Attribute that constructs object lazily.
 *
 * <p>The attribute depends on context (argument of lambda expression).</p>
 *
 * @since 0.1
 */
public final class PhComposite implements Phi {
    /**
     * The argument of the expression.
     */
    private final Phi argument;

    /**
     * The expression itself.
     */
    private final Function<Phi, Phi> expr;

    /**
     * Ctor.
     * @param obj The \rho
     * @param exp The expression
     */
    public PhComposite(final Phi obj, final Function<Phi, Phi> exp) {
        this.argument = obj;
        this.expr = exp;
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhComposite(self, this.expr);
    }

    @Override
    public Phi take(final int pos) {
        return this.expr.apply(this.argument);
    }

    @Override
    public Phi copy() {
        return this.argument.copy();
    }

    @Override
    public boolean hasRho() {
        return this.argument.hasRho();
    }

    @Override
    public Phi take(final String name) {
        return this.expr.apply(this.argument);
    }

    @Override
    public void put(final String name, final Phi object) {
        throw new ExReadOnly(
            "Can't overwrite lambda expression"
        );
    }

    @Override
    public String locator() {
        return this.argument.locator();
    }

    @Override
    public String forma() {
        return this.argument.forma();
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw new ExReadOnly(
            "Can't overwrite lambda expression"
        );
    }

    @Override
    public byte[] delta() {
        return this.argument.delta();
    }
}
