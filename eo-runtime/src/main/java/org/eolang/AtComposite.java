/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
public final class AtComposite implements Attribute {

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
    public AtComposite(final Phi obj, final Function<Phi, Phi> exp) {
        this.argument = obj;
        this.expr = exp;
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtComposite(self, this.expr);
    }

    @Override
    public Phi get() {
        return this.expr.apply(this.argument);
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            "Can't overwrite lazy evaluated attribute"
        );
    }

    @Override
    public boolean vacant() {
        return false;
    }

    @Override
    public String φTerm() {
        return Phi.LAMBDA;
    }
}
