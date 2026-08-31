/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

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
        for (final XML hollow : this.given.nodes("//attr[@void='true']")) {
            found.put(hollow.xpath("@type").get(0), Seen.members(hollow));
        }
        return found;
    }

    private static Collection<Type> members(final XML hollow) {
        final Collection<Type> found = new ArrayList<>(0);
        for (final String loc : hollow.xpath("witnessed/ref/@loc|witnessed/union/ref/@loc")) {
            found.add(new Ref(loc));
        }
        if (!hollow.nodes("witnessed/data|witnessed/union/data").isEmpty()) {
            found.add(new Data());
        }
        if (!hollow.nodes("witnessed/unknown").isEmpty()) {
            found.add(new Unknown());
        }
        return found;
    }
}
