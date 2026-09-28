/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * Where a run of names arrives, asked of an object.
 *
 * <p>A name in a locator is an attribute of whatever the name before it
 * arrived at, so the run is walked one name at a time rather than looked up
 * whole: the {@code if.eq} of a {@code Φ.bool} is the {@code eq} of whatever
 * its {@code if} turned out to be, and that is an object with a locator of its
 * own. A name that arrives nowhere ends the walk and nothing comes back, since
 * half of a question is not an answer to it.</p>
 *
 * @since 0.71.0
 */
final class Arrived {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     */
    Arrived(final Provided provided) {
        this.owned = provided;
    }

    /**
     * Where this run of names arrives, asked of this object.
     *
     * @param start The locator of the object the names are asked of
     * @param path The names, one after another, separated by dots
     * @return The locator, the object itself where the path is empty, and
     *  empty where one of the names arrives nowhere
     */
    String names(final String start, final String path) {
        String walked = start;
        int from = 0;
        while (from < path.length() && !walked.isEmpty()) {
            int next = path.indexOf('.', from);
            if (next < 0) {
                next = path.length();
            }
            walked = this.owned.attribute(walked, path.substring(from, next));
            from = next + 1;
        }
        return walked;
    }

    /**
     * Where this run of names arrives, asked of every one of these objects.
     *
     * <p>All of them or none of them. These objects stand for one object
     * nobody has pinned down, and an answer that holds for some of them says
     * nothing about the one they stand for.</p>
     *
     * @param starts The locators of the objects the names are asked of
     * @param path The names, one after another, separated by dots
     * @return The locators, empty where the path arrives nowhere from one of
     *  the objects
     */
    Collection<String> names(final Collection<String> starts, final String path) {
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final String start : starts) {
            final String arrived = this.names(start, path);
            if (arrived.isEmpty()) {
                found.clear();
                break;
            }
            found.add(arrived);
        }
        return found;
    }
}
