/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Every name the program asks, gathered on the object that answers it.
 *
 * <p>{@link Needs} writes a row per dispatch, against the site that did the
 * asking, since that is what it sees. The same fact is worth having the other
 * way round — everything ever asked of one object, in one place — and nobody
 * can turn the table around until the links say what each of those asks
 * arrived at.</p>
 *
 * <p>Which object a name is asked of is not the receiver, and the difference
 * is the whole point. {@code book.size} asks a {@code book}, and a
 * {@code book} that binds no {@code size} hands the question to the void it
 * keeps, so it is that void the name is really asked of. The links have
 * already worked this out — the dispatch turns out to be
 * {@code Φ.book.pages.size} — and the object that answers is that name
 * without its last step, which is why an answer that does not end in the name
 * asked is left alone: nothing there says whose it is.</p>
 *
 * @since 0.69.0
 */
final class Asked {

    /**
     * The needs table.
     */
    private final XML wanted;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     * @param needs The needs table, as {@link Needs} wrote it
     * @param aliases The name every type goes by, from {@link Ends}
     */
    Asked(final XML needs, final Map<String, String> aliases) {
        this.wanted = needs;
        this.names = aliases;
    }

    /**
     * What is asked of every object of the program.
     * @return The names asked, by the object they are asked of, each against
     *  the object that answers it
     */
    Map<String, Map<String, String>> all() {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final XML attr : this.wanted.nodes("/needs/type/attr")) {
            final String name = attr.xpath("@name").get(0);
            final String step = ".".concat(name);
            final String answer = this.names.getOrDefault(attr.xpath("@type").get(0), "");
            if (answer.endsWith(step)) {
                found.computeIfAbsent(
                    answer.substring(0, answer.length() - step.length()),
                    key -> new LinkedHashMap<>(0)
                ).put(name, answer);
            }
        }
        return Collections.unmodifiableMap(found);
    }
}
