/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
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
 * <p>A copy of somebody else's void carries the arms of the call as well,
 * where the call reaches formations that hand back what they were given and
 * those hand back different things. The locator is then true of every caller
 * and concrete for none, and the arms are the whole of what this one caller
 * may come back with, so the two belong in one row and the choice goes inside
 * the copy it refines (#8744).</p>
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
     * The objects the call may come back with, where the locator is a void.
     */
    private final Collection<String> arms;

    /**
     * Ctor.
     *
     * @param target The locator of what this object is a copy of
     */
    Ref(final String target) {
        this(target, Collections.emptyMap());
    }

    /**
     * Ctor.
     *
     * @param target The locator of what this object is a copy of
     * @param binds What this copy has put into the voids, by the locator of
     *  the void, in the order the voids were declared
     */
    Ref(final String target, final Map<String, String> binds) {
        this(target, binds, Collections.emptyList());
    }

    /**
     * Ctor.
     *
     * @param target The locator of what this object is a copy of
     * @param binds What this copy has put into the voids, by the locator of
     *  the void, in the order the voids were declared
     * @param chosen The objects the call may come back with, empty where it
     *  comes back with one of them or with none
     */
    Ref(
        final String target,
        final Map<String, String> binds,
        final Collection<String> chosen
    ) {
        this.loc = target;
        this.filled = binds;
        this.arms = chosen;
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
        if (!this.arms.isEmpty()) {
            final Collection<Type> members = new ArrayList<>(this.arms.size());
            for (final String arm : this.arms) {
                members.add(new Ref(arm));
            }
            dirs.append(new Union(members).directives());
        }
        return dirs.up();
    }
}
