/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Object which constructs inner object lazily.
 *
 * @checkstyle DesignForExtensionCheck (100 lines)
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
public class PhOnce implements Phi {
    /**
     * Cache.
     */
    private final AtomicReference<Phi> ref;

    /**
     * The object fetched.
     */
    private final Function<Phi, Phi> expr;

    /**
     * Argument.
     */
    private final Phi arg;

    /**
     * Ctor.
     * @param obj The object
     */
    public PhOnce(final Supplier<Phi> obj) {
        this(null, phi -> obj.get());
    }

    /**
     * Ctor.
     * @param obj  Argument for expression
     * @param func Expression
     */
    public PhOnce(final Phi obj, final Function<Phi, Phi> func) {
        this.ref = new AtomicReference<>(null);
        this.arg = obj;
        this.expr = phi -> {
            synchronized (this.ref) {
                this.ref.compareAndSet(null, func.apply(phi));
                return this.ref.get();
            }
        };
    }

    @Override
    public boolean equals(final Object obj) {
        return this.expr.apply(this.arg).equals(obj);
    }

    @Override
    public int hashCode() {
        return this.expr.apply(this.arg).hashCode();
    }

    @Override
    public Phi copy() {
        return this.copy(this.arg);
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhOnce(self, this.expr);
    }

    @Override
    public Phi take(final String name) {
        return this.expr.apply(this.arg).take(name);
    }

    @Override
    public Phi take(final int pos) {
        return this.expr.apply(this.arg).take(pos);
    }

    @Override
    public void put(final int pos, final Phi obj) {
        this.expr.apply(this.arg).put(pos, obj);
    }

    @Override
    public void put(final String name, final Phi obj) {
        this.expr.apply(this.arg).put(name, obj);
    }

    @Override
    public String locator() {
        return this.expr.apply(this.arg).locator();
    }

    @Override
    public String forma() {
        return this.expr.apply(this.arg).forma();
    }

    @Override
    public boolean hasRho() {
        return this.expr.apply(this.arg).hasRho();
    }

    @Override
    public byte[] delta() {
        return this.expr.apply(this.arg).delta();
    }
}
