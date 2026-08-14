/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/**
 * How deeply an object of a program is understood.
 *
 * <p>Knowing something about an object is not one thing, and counting the
 * objects we wrote a row about says almost nothing on its own. That
 * {@code Φ.inc.x.next} is the {@code next} of whatever fills {@code x} is a
 * fact, and a shallower one than that an object is a {@code Φ.number}, which
 * is itself shallower than a copy with no void left free. So the objects go on
 * a ladder and the rungs are counted apart.</p>
 *
 * <p>The rungs run: nothing at all; a name rooted at a void, true of every
 * caller and concrete for none; a formation with voids still free, so it is
 * known which object this is but not what it holds; a formation with nothing
 * left free; and, at the top, an object with nothing left to find out about
 * it. A formation needs no rung of its own: it is a copy of itself that has
 * filled none of its own voids, and lands where that puts it.</p>
 *
 * <p>A datum goes straight to the top. The bytes of a literal are the ground
 * the program stands on, and asking what more there is to know about
 * {@code 01-} is asking nothing.</p>
 *
 * @since 0.69.0
 */
final class Rung {

    /**
     * The rows of the provides table, by the locator of their owner.
     */
    private final Map<String, Collection<Map<String, String>>> table;

    /**
     * The locator of every void.
     */
    private final Collection<String> hollows;

    /**
     * The objects that are data.
     */
    private final Collection<String> ground;

    /**
     * Every chain of copies, walked to its end, from {@link Ends}.
     */
    private final Map<String, String> ends;

    /**
     * Ctor.
     * @param rows The rows of the provides table, by the locator of their
     *  owner, from {@link Ungrouped}
     * @param voids The locator of every void
     * @param data The objects that are data
     * @param names Every chain of copies, walked to its end
     */
    Rung(
        final Map<String, Collection<Map<String, String>>> rows,
        final Collection<String> voids,
        final Collection<String> data,
        final Map<String, String> names
    ) {
        this.table = rows;
        this.hollows = voids;
        this.ground = data;
        this.ends = names;
    }

    /**
     * The rung this object stands on.
     * @param locator The locator of the object
     * @param filled How many voids this object has filled itself
     * @return The rung, from nothing at all up to nothing left to find out
     */
    int reached(final String locator, final int filled) {
        final String end = this.ends.getOrDefault(locator, locator);
        final int found;
        if (this.ground.contains(end)) {
            found = 4;
        } else if (this.table.containsKey(end)) {
            found = this.depth(end, this.voids(end) - filled);
        } else if (this.rooted(end)) {
            found = 1;
        } else {
            found = 0;
        }
        return found;
    }

    /**
     * How deeply a copy of this type is understood.
     * @param type The locator of the type
     * @param free How many of its voids nobody has filled here
     * @return The rung
     */
    private int depth(final String type, final int free) {
        final int found;
        if (free > 0) {
            found = 2;
        } else if (this.whole(type)) {
            found = 4;
        } else {
            found = 3;
        }
        return found;
    }

    /**
     * How many voids this type declares.
     * @param type The locator of the type
     * @return The voids
     */
    private int voids(final String type) {
        int found = 0;
        for (final Map<String, String> row : this.own(type)) {
            if ("true".equals(row.get("void"))) {
                found = found + 1;
            }
        }
        return found;
    }

    /**
     * Whether every attribute of this type was seen.
     * @param type The locator of the type
     * @return True when nothing about it is left to find out
     */
    private boolean whole(final String type) {
        boolean found = false;
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("id")) {
                found = "true".equals(row.get("complete"));
            }
        }
        return found;
    }

    /**
     * Whether this locator is a name taken from a void.
     * @param locator The locator
     * @return True when nothing but a caller can say what it is
     */
    private boolean rooted(final String locator) {
        boolean found = false;
        String walked = locator;
        while (!walked.isEmpty()) {
            if (this.hollows.contains(walked)) {
                found = true;
                break;
            }
            if (!walked.contains(".")) {
                break;
            }
            walked = walked.substring(0, walked.lastIndexOf('.'));
        }
        return found;
    }

    /**
     * The rows about the type of the given locator.
     * @param type The locator of the type
     * @return The rows, empty when the table says nothing about it
     */
    private Collection<Map<String, String>> own(final String type) {
        return this.table.getOrDefault(type, Collections.emptyList());
    }
}
