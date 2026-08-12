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
 * up to the top. Nothing else is consulted — an attribute reachable only
 * through {@code φ} or through a package member is not found here, and the
 * name simply stays unresolved. The checker is allowed to know less; it is
 * not allowed to guess, because a wrong link would make every later answer
 * wrong with it.</p>
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
     * @param all The locator of every object of the program
     * @param made The locators of its formations
     */
    Scope(final Collection<String> all, final Collection<String> made) {
        this.locators = all;
        this.formations = made;
    }

    /**
     * The locator the given reference points at.
     * @param reference The locator of the reference itself
     * @param base The base it carries, {@code ξ.t} or {@code Φ.number}
     * @return The locator, or an empty string when the name is not found
     */
    String target(final String reference, final String base) {
        final String found;
        if (base.startsWith("Φ.")) {
            found = this.rooted(base);
        } else if ("ξ.ρ".equals(base)) {
            found = this.around(this.around(reference));
        } else {
            found = this.outwards(reference, base.substring(base.indexOf('.') + 1));
        }
        return found;
    }

    /**
     * The root object of the given name, if the program has one.
     * @param base The base, which is the locator of a root object itself
     * @return The locator, or an empty string
     */
    private String rooted(final String base) {
        final String found;
        if (this.locators.contains(base)) {
            found = base;
        } else {
            found = "";
        }
        return found;
    }

    /**
     * The nearest formation around the given locator.
     *
     * <p>Applied to a reference it answers with the object being formed
     * around it, which is what {@code ξ} names; applied to that answer it
     * gives what the object sits in, which is what {@code ξ.ρ} names. A
     * top-level object sits in a package rather than in a formation, and
     * nothing is answered about it.</p>
     *
     * @param locator The locator to walk out of
     * @return The locator of the formation, or an empty string when no
     *  formation is around it
     */
    private String around(final String locator) {
        String walked = locator;
        String found = "";
        while (walked.contains(".")) {
            walked = walked.substring(0, walked.lastIndexOf('.'));
            if (this.formations.contains(walked)) {
                found = walked;
                break;
            }
        }
        return found;
    }

    /**
     * The nearest formation around the reference that binds the name.
     * @param reference The locator of the reference itself
     * @param name The name it asks for
     * @return The locator of the attribute, or an empty string
     */
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
