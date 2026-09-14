/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The way from a site inside a fragment to what an atom in its place reads.
 *
 * <p>A void of the fragment is reached up one {@code ρ} per formation
 * between the site and the fragment, and then along the path of the void.
 * A formation entered where it stands, keyed {@code box:<locator>}, is
 * reached from the nearest formation the fragment and it are both inside,
 * or by its full locator when that formation is outside the top-level
 * object, which is a copy the same everywhere.</p>
 *
 * @since 0.77.0
 */
final class Route {

    /**
     * The fragment.
     */
    private final Element fragment;

    /**
     * The site inside it.
     */
    private final Element site;

    /**
     * Ctor.
     *
     * @param formation The fragment
     * @param place The site inside it
     */
    Route(final Element formation, final Element place) {
        this.fragment = formation;
        this.site = place;
    }

    /**
     * The base of a reference from the site to an input.
     *
     * @param key The path of a void, or {@code box:<locator>} of a formation
     * @return The base
     */
    String to(final String key) {
        final String out;
        if (key.startsWith("box:")) {
            final String target = key.substring(4);
            final String place = this.fragment.getAttribute("loc");
            final String common = Route.common(
                place, target.substring(0, target.lastIndexOf('.'))
            );
            final String top = this.top();
            if (common.equals(top) || common.startsWith(String.format("%s.", top))) {
                out = Route.climbed(
                    this.depth() + place.split("\\.", -1).length - common.split("\\.", -1).length,
                    target.substring(common.length() + 1)
                );
            } else {
                out = target;
            }
        } else {
            out = Route.climbed(this.depth(), key);
        }
        return out;
    }

    private String top() {
        Element cursor = this.fragment;
        while (!"object".equals(cursor.getParentNode().getNodeName())) {
            cursor = (Element) cursor.getParentNode();
        }
        return cursor.getAttribute("loc");
    }

    private int depth() {
        int out = 0;
        Node cursor = this.site.getParentNode();
        while (!cursor.equals(this.fragment)) {
            if (!((Element) cursor).hasAttribute("base")) {
                ++out;
            }
            cursor = cursor.getParentNode();
        }
        return out;
    }

    private static String climbed(final int depth, final String path) {
        return String.format(
            "ξ%s.%s", String.join("", Collections.nCopies(depth, ".ρ")), path
        );
    }

    private static String common(final String left, final String right) {
        final String[] one = left.split("\\.", -1);
        final String[] two = right.split("\\.", -1);
        final List<String> out = new ArrayList<>(one.length);
        for (int idx = 0; idx < Math.min(one.length, two.length); ++idx) {
            if (!one[idx].equals(two[idx])) {
                break;
            }
            out.add(one[idx]);
        }
        return String.join(".", out);
    }
}
