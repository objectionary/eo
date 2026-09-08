/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * Where a name points, in a program.
 *
 * <p>A reference names something without saying where it is:
 * {@code ξ.t} means "the {@code t} of the object I am inside of" and
 * {@code Φ.number} means "the {@code number} the whole program knows". This
 * is the object that turns either into the locator it means, which is the
 * first thing in the checker that has to look beyond the object in front of
 * it.</p>
 *
 * <p>A ξ-name is looked for outwards: the nearest formation around the
 * reference that binds it wins, and if none does, the next one out is tried,
 * up to the top. The one exception is {@code ξ.ρ}, written {@code ^}, which
 * names the receiver the formation around it declares rather than anything
 * bound by name. It used to be found by walking two formations out, as the
 * object one step out from the one being formed, and that answer died with
 * #6657: a receiver is now declared or absent, and what it holds is decided
 * by whoever dispatches, so the void itself is the whole of the answer the
 * text can give. A formation that declares none leaves the caret unresolved,
 * since the runtime terminates on the {@code ρ} of such an object and there
 * is nothing to point at. Beyond that, nothing else is consulted — an
 * attribute reachable only through {@code φ} or through a package member is
 * not found here, and the name simply stays
 * unresolved. The checker is allowed to know less; it is not allowed to
 * guess, because a wrong link would make every later answer wrong with
 * it.</p>
 *
 * @since 0.68.0
 */
final class Scope {

    /**
     * The locator of every object the program contains.
     */
    private final Collection<String> locators;

    /**
     * The locators of the formations, the only objects that bind names.
     */
    private final Collection<String> formations;

    /**
     * Ctor.
     *
     * @param all The locator of every object of the program
     * @param made The locators of its formations
     */
    Scope(final Collection<String> all, final Collection<String> made) {
        this.locators = all;
        this.formations = made;
    }

    /**
     * The locator the given reference points at.
     *
     * @param reference The locator of the reference itself
     * @param base The base it carries, {@code ξ.t} or {@code Φ.number}
     * @return The locator, or an empty string when the name is not found
     */
    String target(final String reference, final String base) {
        final String found;
        if (base.startsWith("Φ.")) {
            found = this.rooted(base);
        } else if ("ξ.ρ".equals(base)) {
            found = this.declared(reference);
        } else {
            found = this.outwards(reference, base.substring(base.indexOf('.') + 1));
        }
        return found;
    }

    private String declared(final String reference) {
        final String hollow = String.join(
            ".", new Nesting(this.formations).around(reference), "ρ"
        );
        final String found;
        if (this.locators.contains(hollow)) {
            found = hollow;
        } else {
            found = "";
        }
        return found;
    }

    private String rooted(final String base) {
        final String found;
        if (this.locators.contains(base)) {
            found = base;
        } else {
            found = "";
        }
        return found;
    }

    private String outwards(final String reference, final String name) {
        String around = reference;
        String found = "";
        while (around.contains(".")) {
            around = around.substring(0, around.lastIndexOf('.'));
            final String candidate = String.join(".", around, name);
            if (this.formations.contains(around) && this.locators.contains(candidate)) {
                found = candidate;
                break;
            }
        }
        return found;
    }
}
