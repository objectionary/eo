/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.HashMap;
import java.util.Map;

/**
 * Tags (design doc 3.6): what keeps apart the copies list code makes for
 * different places of user code.
 *
 * <p>A tag starts where code with no tag makes a copy of list code, by a
 * call or a read, and the new copy's tag is the place of that call or read.
 * A tag passes on to every copy tagged code makes, whatever its formation,
 * and it grows by the place of a call where list code of one package makes
 * a copy of list code of another, {@code A/B}, once per package. A copy of
 * an object whose {@code @} is one of its own voids takes no tag, nor does
 * a copy of a formation in a package that gives back new data.</p>
 *
 * @since 0.76.0
 */
final class Tags {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The package of each name, worked out once, empty for a name inside a
     * test.
     */
    private final Map<String, String> packages;

    /**
     * Whether each formation takes no tag, worked out once.
     */
    private final Map<String, Boolean> plain;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Tags(final State walk) {
        this.state = walk;
        this.packages = new HashMap<>(0);
        this.plain = new HashMap<>(0);
    }

    /**
     * The tag of a copy made of a formation by code at a site.
     *
     * @param site The site of the call or read, under the tag of the code
     *  that makes it
     * @param form The formation
     * @return The tag, empty when none
     */
    String tagged(final Site site, final String form) {
        final String tag = site.tag();
        final String place = site.place();
        final String result;
        if (this.untagged(form)) {
            result = "";
        } else if (tag.isEmpty()) {
            if (this.state.names().root(this.pkg(form))) {
                result = place;
            } else {
                result = "";
            }
        } else if (this.state.names().root(this.pkg(form))
            && this.state.names().root(this.pkg(place))
            && !this.pkg(place).equals(this.pkg(form))
            && this.fresh(tag, this.pkg(place))) {
            result = String.join("/", tag, place);
        } else {
            result = tag;
        }
        return result;
    }

    /**
     * Where a copy of a formation made by code at a site is born: at the
     * same place, under the tag it takes.
     *
     * @param site The site of the call or read
     * @param form The formation
     * @return The site it is born at
     */
    Site born(final Site site, final String form) {
        return new Site(site.place(), this.tagged(site, form));
    }

    /**
     * The tag a value carries: its own if it is a copy.
     *
     * @param num The value
     * @return The tag, empty when none
     */
    String carried(final int num) {
        final Value value = this.state.values().get(num);
        final String tag;
        if (value.kind() == 'O') {
            tag = value.tag();
        } else {
            tag = "";
        }
        return tag;
    }

    /**
     * The package of a name: its first two parts, or none inside a test.
     *
     * @param name The name
     * @return The package, empty when none
     */
    String pkg(final String name) {
        String found = this.packages.get(name);
        if (found == null) {
            final String[] parts = name.split("\\.", -1);
            boolean test = false;
            for (final String part : parts) {
                if (part.startsWith("p🌵") || part.startsWith("n🌵")) {
                    test = true;
                    break;
                }
            }
            if (test) {
                found = "";
            } else if (parts.length > 1) {
                found = String.join(".", parts[0], parts[1]);
            } else {
                found = parts[0];
            }
            this.packages.put(name, found);
        }
        return found;
    }

    private boolean fresh(final String tag, final String pkg) {
        final String[] links = tag.split("/", -1);
        boolean none = true;
        for (int idx = 1; idx < links.length; ++idx) {
            if (this.pkg(links[idx]).equals(pkg)) {
                none = false;
                break;
            }
        }
        return none;
    }

    private boolean untagged(final String form) {
        return this.plain.computeIfAbsent(
            form,
            key -> !this.state.roles().passes(key).isEmpty() || this.state.names().scalar(key)
        );
    }
}
