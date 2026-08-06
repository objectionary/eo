/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.locks.ReentrantLock;

/**
 * SYNC.
 *
 * <p>Decorates {@code object} and behaves exactly like it, while letting only
 * one thread dispatch on it at a time. A miss on this object descends into its
 * λ and from there into the decoratee, all of it inside {@link PhDefault#take},
 * so holding the lock around that one call is enough to keep two threads out of
 * {@code object} at once. Only the dispatch is guarded: the object that comes
 * back is dataized by the caller, outside the lock.</p>
 *
 * <p>The lock is reentrant, because the dispatch reaches the decoratee through
 * λ and thus takes the same lock twice on one thread. It is carried over by
 * {@link PhDefault#copy}, so that every copy of the same sync guards the same
 * decoratee.</p>
 *
 * <p>Two Sonar rules are suppressed here. {@code java:S2160} asks for an
 * {@code equals} of its own, because the lock is a field this class adds; the
 * lock is not identity though, and {@link PhDefault#equals} already tells the
 * whole truth about which object this is. {@code java:S1192} counts the three
 * mentions of {@code object} and asks for a constant, which every other atom
 * here spells out in the very same way instead.</p>
 *
 * @since 0.74.0
 */
@XmirObject(oname = "sync")
@SuppressWarnings({"java:S1192", "java:S2160"})
public final class EOsync extends PhDefault implements Atom {

    /**
     * The lock that only one thread holds while dispatching.
     */
    private final ReentrantLock lock;

    /**
     * Ctor.
     */
    public EOsync() {
        super(new Attrs(new Attr("object", new AtVoid("object"))));
        this.lock = new ReentrantLock();
    }

    @Override
    public Phi take(final String name) {
        this.lock.lock();
        try {
            return super.take(name);
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public Phi lambda() {
        return this.take("object");
    }
}
