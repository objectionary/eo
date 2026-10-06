/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * A literal object with its data already known.
 *
 * <p>The original object remains responsible for every operation except
 * dataization. This keeps the prototype, bindings, receiver behavior and
 * diagnostics unchanged while avoiding the dispatch chain on the hot path.</p>
 *
 * @since 0.1
 */
public final class PhLiteral implements Phi {

    /**
     * The original object graph.
     */
    private final Phi origin;

    /**
     * Immutable literal bytes.
     */
    private final Snapshot data;

    /**
     * New literal wrapper.
     *
     * @param phi Original object graph
     * @param bytes Literal bytes
     */
    public PhLiteral(final Phi phi, final byte[] bytes) {
        this(phi, new Snapshot(bytes));
    }

    /**
     * New wrapper sharing an immutable snapshot.
     *
     * @param phi Original object graph
     * @param snapshot Literal bytes
     */
    private PhLiteral(final Phi phi, final Snapshot snapshot) {
        this.origin = phi;
        this.data = snapshot;
    }

    @Override
    public Phi copy() {
        return new PhLiteral(this.origin.copy(), this.data);
    }

    @Override
    public boolean needsRho() {
        return this.origin.needsRho();
    }

    @Override
    public Phi take(final String name) {
        return this.origin.take(name);
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.origin.put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public byte[] delta() {
        if (Thread.currentThread().isInterrupted()) {
            throw new ExInterrupted(
                "Can't dataize a literal, because the thread was interrupted"
            );
        }
        return this.data.bytes();
    }

    @Override
    public Phi normalized() {
        final Phi normal = this.origin.normalized();
        final Phi result;
        if (normal instanceof PhTerminator) {
            result = normal;
        } else if (normal == this.origin) {
            result = this;
        } else {
            result = new PhLiteral(normal, this.data);
        }
        return result;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public boolean equals(final Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this) + 1;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }
}
