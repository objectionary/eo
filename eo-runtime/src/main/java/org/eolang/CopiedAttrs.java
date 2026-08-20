/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The attributes of a copy, taken out of the origin one at a time.
 *
 * <p>Copying an object used to rebuild every attribute it had, so a wide
 * object paid for all of them even when the copy went on to read a single
 * one. Here the attributes stay where they were created, and only the one
 * that is asked for is copied out of the origin, once, and remembered. A
 * copy of an attribute is cheap — a fresh wrapper around the very same
 * expression — so nothing travels but the binding to the new owner.</p>
 *
 * @since 0.63
 */
final class CopiedAttrs extends AbstractMap<String, Attribute> {

    /**
     * Attributes of the object this one was copied from.
     */
    private final Map<String, Attribute> origin;

    /**
     * The object these attributes belong to.
     */
    private final Phi owner;

    /**
     * The ones taken out of the origin so far.
     */
    private final Bindings taken;

    /**
     * Guards {@link #taken} against concurrent readers.
     */
    private final ReentrantLock lock;

    /**
     * Ctor.
     * @param from Attributes of the origin object
     * @param phi The object these attributes belong to
     */
    CopiedAttrs(final Map<String, Attribute> from, final Phi phi) {
        super();
        this.origin = from;
        this.owner = phi;
        this.taken = new Bindings();
        this.lock = new ReentrantLock();
    }

    @Override
    public boolean containsKey(final Object key) {
        this.lock.lock();
        try {
            return this.taken.containsKey(key) || this.origin.containsKey(key);
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public Attribute get(final Object key) {
        this.lock.lock();
        try {
            final Attribute ready = this.taken.get(key);
            final Attribute attr;
            if (ready == null) {
                attr = this.copied(key);
            } else {
                attr = ready;
            }
            return attr;
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public Attribute put(final String key, final Attribute value) {
        this.lock.lock();
        try {
            return this.taken.put(key, value);
        } finally {
            this.lock.unlock();
        }
    }

    @Override
    public Set<Map.Entry<String, Attribute>> entrySet() {
        this.lock.lock();
        try {
            for (final String key : this.origin.keySet()) {
                this.taken.put(key, this.get(key));
            }
            return this.taken.entrySet();
        } finally {
            this.lock.unlock();
        }
    }

    private Attribute copied(final Object key) {
        final Attribute source = this.origin.get(key);
        final Attribute copy;
        if (source == null) {
            copy = null;
        } else {
            copy = source.copy(this.owner);
            this.taken.put((String) key, copy);
        }
        return copy;
    }
}
