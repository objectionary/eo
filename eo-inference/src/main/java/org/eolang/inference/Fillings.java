/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * What the program puts into every void, read back from the links.
 *
 * <p>Every application says what it fills, and says it in its own row, where
 * {@link Bound} wrote it: a {@code bind} names the void and what went into it.
 * So the fact is in the tables already and lies the wrong way round — to learn
 * what one void is ever given, a reader walks the row of every object that
 * ever filled anything, and eo-runtime has 22,818 of those bindings.</p>
 *
 * <p>What goes in is gathered as a type rather than as a locator, which is
 * what makes the answer worth reading: {@code Φ.bytes} hands its answers to a
 * void filled 7,355 times, and all but one of those fillings is a literal. As
 * types there are two of them, a datum and a {@code Φ.bytes.as-bytes}; as
 * locators there are 7,355.</p>
 *
 * @since 0.69.0
 */
final class Fillings {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     */
    Fillings(final XML links) {
        this.table = links;
    }

    /**
     * What is ever put into every void.
     * @return The types put in, by the locator of the void, without the voids
     *  nobody ever fills
     */
    Map<String, Collection<Type>> all() {
        final Map<String, String> names = new Ends(new Pairs(this.table).all()).names();
        final Forms forms = new Forms(this.table);
        final Map<String, Map<String, Type>> found = new LinkedHashMap<>(0);
        for (final XML bind : this.table.nodes("/links/type/ref/bind")) {
            final List<String> given = bind.xpath("ref/@loc");
            if (!given.isEmpty()) {
                final String end = names.getOrDefault(given.get(0), given.get(0));
                found.computeIfAbsent(
                    bind.xpath("@void").get(0), key -> new LinkedHashMap<>(0)
                ).putIfAbsent(forms.name(end), forms.type(end));
            }
        }
        final Map<String, Collection<Type>> joined = new LinkedHashMap<>(found.size());
        for (final Map.Entry<String, Map<String, Type>> hollow : found.entrySet()) {
            joined.put(hollow.getKey(), hollow.getValue().values());
        }
        return joined;
    }
}
