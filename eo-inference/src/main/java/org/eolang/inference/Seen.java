/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * What callers were seen putting into every void of a program.
 *
 * <p>{@link Witnessed} writes this down, void by void, and here it is read
 * back. A void that nobody fills is still a void and still belongs in the
 * answer, so every one of them is listed, with an empty choice where the
 * table says nothing.</p>
 *
 * <p>The choice is flattened as it is read. A void filled from one place
 * carries its type on its own, a void filled from several carries a
 * {@code union} of them, and to whoever asks what went in the difference
 * between the two is no difference at all.</p>
 *
 * <p>This is evidence and never a contract, exactly as {@link Witnessed}
 * says: nothing may work out the type of a void from what is read here.
 * It is read so that a reader who is told an object is whatever
 * {@code Φ.bool.and.x} turns out to be can also be told what the program was
 * seen putting there.</p>
 *
 * @since 0.70.0
 */
final class Seen {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     * @param provides The provides table
     */
    Seen(final XML provides) {
        this.given = provides;
    }

    /**
     * What was seen going into every void.
     * @return The witnesses, by the locator of the void
     */
    Map<String, Collection<Type>> all() {
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            for (final Xnav hollow : Seen.hollows(type)) {
                found.put(new Noted(hollow).says("type"), Seen.members(hollow));
            }
        }
        return found;
    }

    private static List<Xnav> hollows(final Xnav type) {
        return type.elements(Filter.withName("attr"))
            .filter(attr -> "true".equals(new Noted(attr).says("void")))
            .collect(Collectors.toList());
    }

    private static Collection<Type> members(final Xnav hollow) {
        final Collection<Xnav> told = Seen.told(hollow);
        final Collection<Type> found = new ArrayList<>(0);
        for (final Xnav choice : told) {
            if ("ref".equals(Seen.kind(choice))) {
                found.add(new Ref(new Noted(choice).says("loc")));
            }
        }
        if (Seen.holds(told, "data")) {
            found.add(new Data());
        }
        if (Seen.holds(told, "unknown")) {
            found.add(new Unknown());
        }
        return found;
    }

    private static Collection<Xnav> told(final Xnav hollow) {
        final Collection<Xnav> found = new ArrayList<>(0);
        for (final Xnav witnessed : Seen.choices(hollow, "witnessed")) {
            for (final Xnav choice : Seen.choices(witnessed, "union", "ref", "data", "unknown")) {
                if ("union".equals(Seen.kind(choice))) {
                    found.addAll(Seen.choices(choice, "ref", "data", "unknown"));
                } else {
                    found.add(choice);
                }
            }
        }
        return found;
    }

    private static List<Xnav> choices(final Xnav node, final String... names) {
        return node.elements(
            Filter.any(
                Arrays.stream(names).map(Filter::withName).toArray(Filter[]::new)
            )
        ).collect(Collectors.toList());
    }

    private static boolean holds(final Iterable<Xnav> told, final String name) {
        boolean found = false;
        for (final Xnav choice : told) {
            if (name.equals(Seen.kind(choice))) {
                found = true;
                break;
            }
        }
        return found;
    }

    private static String kind(final Xnav choice) {
        return choice.node().getNodeName();
    }
}
