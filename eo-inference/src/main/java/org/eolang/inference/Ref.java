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
 * <p>A copy may be one only because of what the program was seen to put
 * into a void: the call on a void that every caller fills with one formation
 * is a copy of that formation, and its arguments go into that formation's
 * voids. That is evidence and not a contract, since a caller written tomorrow
 * or compiled apart may put another shape there, so the row says so with
 * {@code witnessed="true"}, on the {@code ref} where what it copies was
 * reached that way and on each {@code bind} that was, and a reader in need of
 * a contract leaves those out (#8914).</p>
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
     * Whether what this is a copy of was reached through a void.
     */
    private final boolean guess;

    /**
     * The voids of {@link #filled} filled through what a void was seen to hold.
     */
    private final Collection<String> relays;

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
        this(target, binds, chosen, false, Collections.emptyList());
    }

    /**
     * Ctor.
     *
     * @param target The locator of what this object is a copy of
     * @param binds What this copy has put into the voids, by the locator of
     *  the void, in the order the voids were declared
     * @param chosen The objects the call may come back with, empty where it
     *  comes back with one of them or with none
     * @param witnessed Whether the target was reached through what the
     *  program was seen to put into a void
     * @param relayed The voids among the binds that were filled through what
     *  the program was seen to put into a void
     */
    Ref(
        final String target,
        final Map<String, String> binds,
        final Collection<String> chosen,
        final boolean witnessed,
        final Collection<String> relayed
    ) {
        this.loc = target;
        this.filled = binds;
        this.arms = chosen;
        this.guess = witnessed;
        this.relays = relayed;
    }

    @Override
    public String names() {
        return this.loc;
    }

    @Override
    public Directives directives() {
        final Directives dirs = new Directives().add("ref").attr("loc", this.loc);
        if (this.guess) {
            dirs.attr("witnessed", "true");
        }
        for (final Map.Entry<String, String> bind : this.filled.entrySet()) {
            dirs.add("bind").attr("void", bind.getKey());
            if (this.relays.contains(bind.getKey())) {
                dirs.attr("witnessed", "true");
            }
            dirs.add("ref")
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
