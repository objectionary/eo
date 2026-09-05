/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * Suggests the closest EO object names when an object is not found, so the
 * "Couldn't find object ..." error points the user at what was meant (#4520).
 * The candidates are the EO objects available on the classpath, collected
 * when this object is built.
 * @since 0.74.0
 */
final class ObjectSuggestions {

    /**
     * The EO object names to suggest among.
     */
    private final List<String> names;

    /**
     * Ctor.
     * @param loader The class loader to scan for EO objects
     */
    ObjectSuggestions(final ClassLoader loader) {
        this(new OnClasspath(loader).names());
    }

    /**
     * Ctor.
     * @param names The EO object names to suggest among
     */
    ObjectSuggestions(final List<String> names) {
        this.names = names;
    }

    /**
     * The five closest EO object names for a missing one.
     * @param fqn FQN of the missing object, e.g. {@code Φ.org.eolang.io.std1out}
     * @return A "Did you mean?" block, or an empty string
     */
    String suggest(final String fqn) {
        String target = fqn;
        if (target.startsWith("Φ.")) {
            target = target.substring(2);
        }
        if (target.startsWith("org.eolang.")) {
            target = target.substring("org.eolang.".length());
        }
        final List<Map.Entry<String, Integer>> ranked = new ArrayList<>(0);
        for (final String name : this.names) {
            ranked.add(
                new AbstractMap.SimpleImmutableEntry<>(
                    name, ObjectSuggestions.dist(target, name)
                )
            );
        }
        ranked.sort(Comparator.comparingInt(Map.Entry::getValue));
        final StringBuilder out = new StringBuilder(64);
        for (int idx = 0; idx < Math.min(5, ranked.size()); ++idx) {
            out.append(String.format("%n  - %s", ranked.get(idx).getKey()));
        }
        final String result;
        if (out.length() > 0) {
            result = String.format("%n%nDid you mean?%s", out);
        } else {
            result = "";
        }
        return result;
    }

    private static int dist(final String src, final String tgt) {
        final int[] row = new int[tgt.length() + 1];
        for (int idx = 0; idx <= tgt.length(); ++idx) {
            row[idx] = idx;
        }
        for (int sidx = 1; sidx <= src.length(); ++sidx) {
            int prev = row[0];
            row[0] = sidx;
            for (int tidx = 1; tidx <= tgt.length(); ++tidx) {
                final int cur = row[tidx];
                row[tidx] = Math.min(
                    Math.min(row[tidx] + 1, row[tidx - 1] + 1),
                    prev + ObjectSuggestions.cost(src, tgt, sidx, tidx)
                );
                prev = cur;
            }
        }
        return row[tgt.length()];
    }

    private static int cost(final String src, final String tgt,
        final int sidx, final int tidx) {
        final int result;
        if (src.charAt(sidx - 1) == tgt.charAt(tidx - 1)) {
            result = 0;
        } else {
            result = 1;
        }
        return result;
    }
}
