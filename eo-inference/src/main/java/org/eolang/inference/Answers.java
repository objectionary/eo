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
 * <p>A datum and a termination go straight to the top. The bytes of a literal
 * are the ground the program stands on, and asking what more there is to know
 * about {@code 01-} is asking nothing; an object that never comes back with a
 * value has nothing further to say either.</p>
 *
 * <p>Where the walk stopped is given back along with the rung, in one
 * {@link Answer}, because that locator is the answer a reader wants and it was
 * already worked out on the way to the rung. Whoever counts the program and
 * whoever shows it to somebody read the same one.</p>
 *
 * <p>A void the program fills one way and no other is that one thing and
 * climbs the ladder as that thing, which is {@link Sole}'s question. A void
 * that keeps itself stays on the rung where a name rooted at a void belongs,
 * true of every caller and concrete for none.</p>
 *
 * <p>Where the void keeps itself, what {@link Seen} found in it goes back
 * beside it, for a reader who is told their object is whatever
 * {@code Φ.bool.and.x} turns out to be and would rather be told that
 * {@code Φ.true} and {@code Φ.false} have both been put there.</p>
 *
 * @since 0.69.0
 */
final class Answers {

    /**
     * The rows of the provides table, by the locator of their owner.
     */
    private final Map<String, Collection<Map<String, String>>> table;

    /**
     * Every void, with what the program was seen putting into it.
     */
    private final Map<String, Collection<Type>> hollows;

    /**
     * The objects the table answers by itself.
     */
    private final Collection<String> ground;

    /**
     * Where the answer for every object is to be looked for.
     */
    private final Map<String, String> ends;

    /**
     * Ctor.
     * @param rows The rows of the provides table, by the locator of their
     *  owner, from {@link Ungrouped}
     * @param voids Every void, with what the program was seen putting into
     *  it, from {@link Seen}
     * @param answered The objects the table answers by itself, from
     *  {@link Pairs}
     * @param names Where the answer for every object is to be looked for,
     *  which is the end of its chain of copies, and for the body of an atom
     *  the forma the atom declares
     */
    Answers(
        final Map<String, Collection<Map<String, String>>> rows,
        final Map<String, Collection<Type>> voids,
        final Collection<String> answered,
        final Map<String, String> names
    ) {
        this.table = rows;
        this.hollows = voids;
        this.ground = answered;
        this.ends = names;
    }

    /**
     * What this object turns out to be.
     * @param locator The locator of the object
     * @param filled The locators of the voids this object has filled, its own
     *  and the ones filled earlier in its chain of copies
     * @return The answer, saying what it settled on and how deep that is
     */
    Answer of(final String locator, final Collection<String> filled) {
        final String end = this.ends.getOrDefault(locator, locator);
        final String root = this.root(end);
        final String sole = this.sole(end);
        final Answer found;
        if (this.ground.contains(end)) {
            found = new Answer(end, 4);
        } else if (this.table.containsKey(end)) {
            found = new Answer(end, this.depth(end, this.free(end, filled)));
        } else if (root.isEmpty()) {
            found = new Answer(end, 0);
        } else if (sole.isEmpty()) {
            found = new Answer(end, 1, this.hollows.get(root));
        } else {
            found = new Answer(sole, this.depth(sole, this.free(sole, filled)));
        }
        return found;
    }

    private String sole(final String end) {
        return new Sole(
            this.hollows.getOrDefault(end, Collections.emptyList()), this.table.keySet()
        ).names();
    }

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

    private int free(final String type, final Collection<String> filled) {
        int found = 0;
        for (final Map<String, String> row : this.own(type)) {
            if ("true".equals(row.get("void")) && !"ρ".equals(row.get("name"))
                && !filled.contains(row.getOrDefault("type", ""))) {
                found = found + 1;
            }
        }
        return found;
    }

    private boolean whole(final String type) {
        boolean found = false;
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("id")) {
                found = "true".equals(row.get("complete"));
            }
        }
        return found;
    }

    private String root(final String locator) {
        String found = "";
        String walked = locator;
        while (!walked.isEmpty()) {
            if (this.hollows.containsKey(walked)) {
                found = walked;
                break;
            }
            if (!walked.contains(".")) {
                break;
            }
            walked = walked.substring(0, walked.lastIndexOf('.'));
        }
        return found;
    }

    private Collection<Map<String, String>> own(final String type) {
        return this.table.getOrDefault(type, Collections.emptyList());
    }
}
