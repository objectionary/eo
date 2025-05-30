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
final class PhaLogged implements Phi {

    /**
     * Origin.
     */
    private final Phi origin;

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
    PhaLogged(final Phi attr, final String label) {
        this(attr, label, Logger.getLogger(PhaLogged.class.getName()));
    }

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     * @param logger Logger
     */
    PhaLogged(final Phi attr, final String label, final Logger logger) {
        this.origin = attr;
        this.owner = label;
        this.log = logger;
    }

    @Override
    public Phi copy(final Phi self) {
        this.log.info(String.format("  %s.copy()...\n", this.owner));
        final Phi ret = this.origin.copy(self);
        this.log.info(String.format("  %s.copy()!\n", this.owner));
        return ret;
    }

    @Override
    public Phi take(final int pos) {
        this.log.info(String.format("  %s.get()...\n", this.owner));
        final Phi ret = this.origin.take(0);
        this.log.info(String.format("  %s.get()! -> %d\n", this.owner, ret.hashCode()));
        return ret;
    }

    @Override
    public Phi copy() {
        return this.origin.copy();
    }

    @Override
    public boolean hasRho() {
        return this.origin.hasRho();
    }

    @Override
    public Phi take(final String name) {
        this.log.info(String.format("  %s.get()...\n", this.owner));
        final Phi ret = this.origin.take(0);
        this.log.info(String.format("  %s.get()! -> %d\n", this.owner, ret.hashCode()));
        return ret;
    }

    @Override
    public void put(final String name, final Phi object) {
        this.log.info(String.format("  %s.put()...\n", this.owner));
        this.origin.put(name, object);
        this.log.info(String.format("  %s.put()!\n", this.owner));
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
    public void put(final int pos, final Phi object) {
        this.log.info(String.format("  %s.put()...\n", this.owner));
        this.origin.put(pos, object);
        this.log.info(String.format("  %s.put()!\n", this.owner));
    }

    @Override
    public byte[] delta() {
        return this.origin.delta();
    }
}
