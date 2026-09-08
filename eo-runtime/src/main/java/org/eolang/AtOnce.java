/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Attribute that retrieves object only once.
 *
 * <p>It's highly recommended to use it with {@link AtComposite}.</p>
 *
 * <p>The first retrieval happens under the monitor of this very attribute,
 * and not under a lock of its own, because an object of the language is made
 * of these attributes and there are millions of them in the heap of a program
 * that runs for a while: a lock costs two objects each, while a monitor
 * costs nothing until two threads want it at the same time.</p>
 *
 * @since 0.1
 * @checkstyle RegexpSinglelineCheck (60 lines)
 */
@SuppressWarnings("PMD.AvoidSynchronizedStatement")
public final class AtOnce implements Attribute {

    /**
     * Origin attribute.
     */
    private final Attribute origin;

    /**
     * Cache, {@code null} until the origin is retrieved.
     */
    private volatile Phi cached;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    public AtOnce(final Attribute attr) {
        this.origin = attr;
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        Phi ret = this.cached;
        if (ret == null) {
            synchronized (this) {
                if (this.cached == null) {
                    this.cached = this.origin.get();
                }
                ret = this.cached;
            }
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            String.format(
                "Can't overwrite the cached attribute \"%s\"",
                this.origin
            )
        );
    }

    @Override
    public boolean vacant() {
        return false;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
