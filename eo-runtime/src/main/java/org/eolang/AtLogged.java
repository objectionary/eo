/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.logging.Logger;

/**
 * An attribute that logs all its operations to the console (very
 * convenient for debugging).
 *
 * @since 0.24
 */
final class AtLogged implements Attr {

    /**
     * Origin.
     */
    private final Attr origin;

    /**
     * Owner.
     */
    private final String owner;

    /**
     * Logger.
     */
    private final Logger log;

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     */
    AtLogged(final Attr attr, final String label) {
        this(attr, label, Logger.getLogger(AtLogged.class.getName()));
    }

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     * @param logger Logger
     */
    AtLogged(final Attr attr, final String label, final Logger logger) {
        this.origin = attr;
        this.owner = label;
        this.log = logger;
    }

    @Override
    public Attr copy(final Phi self) {
        this.log.info(String.format("  %s.copy()...\n", this.owner));
        final Attr ret = this.origin.copy(self);
        this.log.info(String.format("  %s.copy()!\n", this.owner));
        return ret;
    }

    @Override
    public Phi get() {
        this.log.info(String.format("  %s.get()...\n", this.owner));
        final Phi ret = this.origin.get();
        this.log.info(String.format("  %s.get()! -> %d\n", this.owner, ret.hashCode()));
        return ret;
    }

    @Override
    public void put(final Phi src) {
        this.log.info(String.format("  %s.put()...\n", this.owner));
        this.origin.put(src);
        this.log.info(String.format("  %s.put()!\n", this.owner));
    }
}
