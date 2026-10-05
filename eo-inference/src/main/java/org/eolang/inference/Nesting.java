/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * What a locator is written inside.
 *
 * <p>A locator is the path to where an object is written, so walking it back
 * one name at a time and stopping at the first formation is how the object
 * something is written inside is found, and nobody has to be asked. Applied to
 * a reference it answers with the object being formed around it, which is what
 * {@code ξ} names.</p>
 *
 * <p>It does not answer {@code ξ.ρ}. It did, once, by walking out one formation
 * further, and that was the object something sits in. Since #6657 a receiver is
 * declared or absent and a caller decides what it holds, so where the text sits
 * says nothing about it.</p>
 *
 * <p>A top-level object is written inside a package rather than a formation,
 * and nothing is answered about it here.</p>
 *
 * @since 0.69.0
 */
final class Nesting {

    /**
     * The locators of the formations, the only objects anything sits in.
     */
    private final Collection<String> formations;

    /**
     * Ctor.
     *
     * @param made The locators of the formations of the program
     */
    Nesting(final Collection<String> made) {
        this.formations = made;
    }

    /**
     * The formation this locator is written inside.
     *
     * @param locator The locator to walk out of
     * @return The locator of the formation, or an empty string when no
     *  formation is around it
     */
    String around(final String locator) {
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
}
