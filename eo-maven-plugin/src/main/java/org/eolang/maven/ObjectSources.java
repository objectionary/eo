/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Map;

/**
 * Resolves the {@code .eo} source file for a {@code PhCoverage} location's
 * symbolic locator, by matching the locator's leading object id against the
 * already-existing {@code eo-foreign} catalog (every registered object is
 * already mapped there to the source it was compiled from), rather than
 * having the transpiler duplicate that same mapping into a manifest of its
 * own for every single instrumented location.
 * @since 0.58
 */
final class ObjectSources {

    /**
     * Every locator this codebase emits starts with the root object marker.
     */
    private static final String ROOT = "Φ.";

    /**
     * Object id (as in {@code eo-foreign}, e.g. {@code string.scanf}) mapped
     * to the source it was compiled from.
     */
    private final Map<String, String> ids;

    /**
     * Ctor.
     * @param registered Object id mapped to its source, as in {@code eo-foreign}
     */
    ObjectSources(final Map<String, String> registered) {
        this.ids = registered;
    }

    /**
     * The source of the object a location's locator belongs to.
     * @param locator The locator, e.g. {@code Φ.string.scanf.tokenize}
     * @return The source
     */
    String source(final String locator) {
        String rest = locator;
        if (rest.startsWith(ObjectSources.ROOT)) {
            rest = rest.substring(ObjectSources.ROOT.length());
        }
        String longest = null;
        for (final String id : this.ids.keySet()) {
            if (ObjectSources.owns(rest, id) && ObjectSources.longer(longest, id)) {
                longest = id;
            }
        }
        if (longest == null) {
            throw new IllegalStateException(
                String.format(
                    "No object in the eo-foreign catalog matches locator '%s'", locator
                )
            );
        }
        return this.ids.get(longest);
    }

    /**
     * Does the given object id own the given (already root-stripped) locator
     * remainder, i.e. is the remainder the id itself or one of its nested
     * attributes?
     * @param rest The locator, with the leading root marker already stripped
     * @param id The candidate object id
     * @return True if it owns it
     */
    private static boolean owns(final String rest, final String id) {
        return rest.equals(id) || rest.startsWith(String.format("%s.", id));
    }

    /**
     * Is the candidate id a longer (more specific) match than the current
     * best, or is there no best yet?
     * @param best The current best id, or NULL when there is none yet
     * @param candidate The candidate id
     * @return True if the candidate should replace the current best
     */
    private static boolean longer(final String best, final String candidate) {
        final boolean result;
        if (best == null) {
            result = true;
        } else {
            result = candidate.length() > best.length();
        }
        return result;
    }
}
