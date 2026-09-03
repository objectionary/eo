/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.Map;
import org.xembly.Directives;

/**
 * An object that is a copy of another one.
 *
 * <p>The commonest answer there is, and the only one that carries anything
 * with it: which voids of what it copies this copy has filled, and with
 * what. A copy that has filled none is written the same way with nothing
 * inside it, which is how a reader tells a saturated copy from one that is
 * still waiting for arguments.</p>
 *
 * @since 0.69.0
 */
final class Ref implements Type {

    /**
     * The locator of what this object is a copy of.
     */
    private final String loc;

    /**
     * What this copy has put into the voids, by the locator of the void.
     */
    private final Map<String, String> filled;

    /**
     * Ctor.
     * @param target The locator of what this object is a copy of
     */
    Ref(final String target) {
        this(target, Collections.emptyMap());
    }

    /**
     * Ctor.
     * @param target The locator of what this object is a copy of
     * @param binds What this copy has put into the voids, by the locator of
     *  the void, in the order the voids were declared
     */
    Ref(final String target, final Map<String, String> binds) {
        this.loc = target;
        this.filled = binds;
    }

    @Override
    public String names() {
        return this.loc;
    }

    @Override
    public Directives directives() {
        final Directives dirs = new Directives().add("ref").attr("loc", this.loc);
        for (final Map.Entry<String, String> bind : this.filled.entrySet()) {
            dirs.add("bind")
                .attr("void", bind.getKey())
                .add("ref")
                .attr("loc", bind.getValue())
                .up()
                .up();
        }
        return dirs.up();
    }
}
