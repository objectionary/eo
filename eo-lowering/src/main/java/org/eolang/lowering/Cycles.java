/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The helpers of a formation that apply themselves, directly or through
 * one another.
 *
 * <p>Such a helper cannot be read in place where it is named, since the
 * reading would never end; it is a body of its own instead, resumed
 * where it is named. Which helpers those are is a question of the
 * names: every synthetic {@code a🌵} name standing anywhere under a
 * helper is a helper it may reach, and a helper that reaches itself
 * along such names takes part in a cycle. A name a nested helper spells
 * counts for the helper it is nested in, since nothing outside that
 * helper can reach the nested one, so the answer may name a helper
 * whose cycle runs through a nested body; it then resumes rather than
 * reads, which is as sound, and only costs the lowering of a call of it
 * outside a tail position. Only a formation can be a body, since only a
 * formation has voids to carry across; an application in a cycle is
 * left to the reading, which refuses it as the cycle it is.</p>
 *
 * @since 0.76.0
 */
public final class Cycles {

    /**
     * What a synthetic name looks like.
     */
    private static final Pattern NAME = Pattern.compile("a🌵[0-9]+-[0-9]+");

    /**
     * The helpers: names to their {@code <o/>} elements.
     */
    private final Map<String, Xnav> helpers;

    /**
     * Ctor.
     * @param bound The helpers: names to their {@code <o/>} elements
     */
    public Cycles(final Map<String, Xnav> bound) {
        this.helpers = bound;
    }

    /**
     * The names of the helpers in a cycle.
     * @return The names, in the order the helpers are bound
     */
    public Collection<String> names() {
        final Map<String, Set<String>> edges = new LinkedHashMap<>();
        for (final Map.Entry<String, Xnav> entry : this.helpers.entrySet()) {
            final Set<String> named = new LinkedHashSet<>();
            Cycles.named(entry.getValue(), named);
            named.retainAll(this.helpers.keySet());
            edges.put(entry.getKey(), named);
        }
        final Collection<String> out = new LinkedHashSet<>();
        for (final String name : edges.keySet()) {
            if (!this.helpers.get(name).attribute("base").text().isPresent()
                && Cycles.reaches(name, name, edges, new HashSet<>())) {
                out.add(name);
            }
        }
        return out;
    }

    private static boolean reaches(final String from, final String target,
        final Map<String, Set<String>> edges, final Set<String> seen) {
        boolean out = false;
        for (final String next : edges.getOrDefault(from, Collections.emptySet())) {
            if (next.equals(target)) {
                out = true;
            } else if (seen.add(next)) {
                out = Cycles.reaches(next, target, edges, seen);
            }
            if (out) {
                break;
            }
        }
        return out;
    }

    private static void named(final Xnav node, final Set<String> into) {
        final Matcher found = Cycles.NAME.matcher(node.attribute("base").text().orElse(""));
        while (found.find()) {
            into.add(found.group());
        }
        node.elements(Filter.withName("o")).forEach(kid -> Cycles.named(kid, into));
    }
}
