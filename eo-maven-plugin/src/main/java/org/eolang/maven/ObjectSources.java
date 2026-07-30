/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Map;
import java.util.Optional;

/**
 * Resolves the {@code .eo} source file for a {@code PhCoverage} location's
 * symbolic locator, by matching the locator's leading object id against the
 * already-existing {@code eo-foreign} catalog (every registered object is
 * already mapped there to the source it was compiled from), rather than
 * having the transpiler duplicate that same mapping into a manifest of its
 * own for every single instrumented location.
 * @since 0.62.0
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
     * <p>
     *     A locator is a dot-separated path down from the object it belongs
     *     to, so the id being looked for is the longest prefix of it that the
     *     catalog knows. Dropping the trailing segment one at a time and
     *     asking the catalog about what is left finds that prefix in as many
     *     lookups as the locator has segments, instead of walking the whole
     *     catalog once per locator.
     * </p>
     * @param locator The locator, e.g. {@code Φ.string.scanf.tokenize}
     * @return The source, or empty when no registered object owns the locator
     */
    Optional<String> source(final String locator) {
        String rest = locator;
        if (rest.startsWith(ObjectSources.ROOT)) {
            rest = rest.substring(ObjectSources.ROOT.length());
        }
        Optional<String> result = Optional.empty();
        while (!rest.isEmpty()) {
            if (this.ids.containsKey(rest)) {
                result = Optional.of(this.ids.get(rest));
                break;
            }
            final int dot = rest.lastIndexOf('.');
            if (dot < 0) {
                break;
            }
            rest = rest.substring(0, dot);
        }
        return result;
    }
}
