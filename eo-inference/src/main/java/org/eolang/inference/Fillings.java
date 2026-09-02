/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What the program puts into every void, read back from the links.
 *
 * <p>Every application says what it fills, and says it in its own row, where
 * {@link Bound} wrote it: a {@code bind} names the void and what went into it.
 * So the fact is in the tables already and lies the wrong way round — to learn
 * what one void is ever given, a reader walks the row of every object that
 * ever filled anything, and eo-runtime has 23,871 of those bindings.</p>
 *
 * <p>What goes in is gathered as a type rather than as a locator, which is
 * what makes the answer worth reading: the {@code φ} of {@code Φ.bytes} is
 * filled 7,752 times and all but a handful of those fillings are literals. As
 * types there are two of them, a datum and a {@code Φ.bytes.as-bytes}; as
 * locators there are 7,752.</p>
 *
 * <p>Which type a filling is counted as is {@link Landed}'s question, and it
 * has to be asked of the links rather than of {@link Ends} alone. An argument
 * is written afresh at every call site, so the same expression passed at
 * eleven places is eleven locators no chain of copies joins, and counting them
 * apart makes a void look filled eleven ways when it is filled one way eleven
 * times. Settling each of them first leaves {@code Φ.number.as-bytes} with
 * five members where it had more than a cap's worth, and the five are worth
 * reading.</p>
 *
 * <p>A filling whose walk runs into a void is no type, and it is no silence
 * either: it says this void is handed whatever another void is handed. So it
 * is kept as the variable it is, beside the types the other callers bring,
 * because a void that four callers fill with a {@code Φ.string} and a fifth
 * fills with a void of its own is not the same void as one that four callers
 * fill and nobody else touches.</p>
 *
 * @since 0.69.0
 */
final class Fillings {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     * @param provides The provides table, which says where a filling can land
     */
    Fillings(final XML links, final XML provides) {
        this.table = links;
        this.given = provides;
    }

    /**
     * What is ever put into every void.
     * @return The types put in, by the locator of the void, without the voids
     *  nobody ever fills
     */
    Map<String, Collection<Type>> all() {
        final Pairs pairs = new Pairs(this.table);
        final Map<String, String> names = new Ends(pairs.all()).names();
        final Map<String, String> landings = new Landed(pairs, this.given).all();
        final Forms forms = new Forms(pairs.forms());
        final Map<String, Map<String, Type>> chosen = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<String>> bound : pairs.puts().entrySet()) {
            for (final String put : bound.getValue()) {
                final String end = landings.getOrDefault(put, names.getOrDefault(put, put));
                chosen.computeIfAbsent(bound.getKey(), key -> new LinkedHashMap<>(0))
                    .putIfAbsent(forms.name(end), forms.type(end));
            }
        }
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, Type>> hollow : chosen.entrySet()) {
            found.put(hollow.getKey(), hollow.getValue().values());
        }
        return found;
    }
}
