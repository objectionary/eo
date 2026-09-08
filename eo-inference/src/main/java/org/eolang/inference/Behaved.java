/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What every type of the program behaves as.
 *
 * <p>A formation that binds nothing but a body has no behaviour of its own.
 * {@code Φ.bytes.as-bytes} hands back the object it was dispatched on and does
 * nothing else, so every name it answers to it answers through its {@code @},
 * and telling a reader that their object is a {@code Φ.bytes.as-bytes} gives
 * them a name and nothing besides. It is a {@code Φ.bytes}, and that is what
 * should be printed. Seven hundred objects of eo-runtime settle on the alias
 * instead, which makes it the fifth most common answer in the library and
 * leaves whoever reads the pages working out that the two names mean one
 * thing.</p>
 *
 * <p>So a type is reduced to what stands behind its {@code @} when {@code @}
 * is its only public attribute. Public means readable from outside the object,
 * which is why the exclusions are not a matter of taste: a {@code 🌵}
 * auto-name is kept out of the {@code NAME} token by R-9.2.2 of the parser
 * spec and no source file can write one down, and a name that begins with a
 * {@code +} or a {@code -} is a test attribute, which the syntax forbids
 * anybody to name either. One more public attribute, though, is one more thing
 * a reader loses by being told something else, so an {@code english} that
 * hands back its receiver and binds {@code lower} and {@code upper} besides
 * keeps its own name.</p>
 *
 * <p>A void is ignored as well, and that clause has an expiry on it. A void is
 * public today, so the rule as it is meant to read — nothing public but
 * {@code @} — would reduce almost nothing; voids are on their way to being
 * named by the parser and unreadable from outside, like a moniker and a test,
 * and the day they get there this clause is deleted and the rule reads as it
 * was written. What it costs meanwhile was measured: of the 1,680 reads that
 * land on a type this reduces, 104 ask for a void the new name has no answer
 * for. Eighty-eight of those are inside a test and eight more ask for
 * {@code ρ}, which leaves eight on the library proper — {@code printf} wanting
 * its {@code args}, {@code with} its {@code key} and its {@code value}, and
 * three besides.</p>
 *
 * <p>The walk goes on for as long as each name it arrives at is as bare as the
 * last, and gives back the final name that still has a row of its own and that
 * a source file could write down, since a void has none and is walked through
 * rather than settled on, and a name with a moniker or a test in it is walked
 * through for the same reason it is not counted: nobody can say it. Trading
 * {@code Φ.i64.plus} for the {@code Φ.i64.plus.a🌵143-4} its body happens to
 * be would hand a reader a worse name than the one they came with. What an atom
 * says it comes back with counts as standing behind {@code @} — the whole of
 * {@code Φ.number.plus} is an annotation saying {@code Φ.number}, so
 * {@code 1.plus 2} is a number — which is {@link Provided}'s
 * {@code behind}, and the rule is that method asked of a row that qualifies
 * until it stops answering.</p>
 *
 * @since 0.71.0
 */
final class Behaved {

    /**
     * The provides table.
     */
    private final XML table;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     *
     * @param provides The provides table, as {@link Provides} wrote it
     * @param aliases The name every type goes by, from {@link Ends}
     */
    Behaved(final XML provides, final Map<String, String> aliases) {
        this.table = provides;
        this.names = aliases;
    }

    /**
     * What every type of the program behaves as.
     *
     * @return The name to go by, by the locator of the type, without the types
     *  that behave as themselves
     */
    Map<String, String> all() {
        final Map<String, Collection<Map<String, String>>> rows =
            new Ungrouped(this.table, Collections.emptyMap()).rows();
        final Provided owned = new Provided(
            this.table, this.names, this.table.xpath("//attr[@void='true']/@type")
        );
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final String type : rows.keySet()) {
            final String behaves = Behaved.walked(type, rows, owned);
            if (!behaves.equals(type)) {
                found.put(type, behaves);
            }
        }
        return found;
    }

    private static String walked(
        final String type,
        final Map<String, Collection<Map<String, String>>> rows,
        final Provided owned
    ) {
        final Collection<String> walked = new HashSet<>(0);
        String found = type;
        String hop = type;
        while (walked.add(hop)
            && Behaved.bare(rows.getOrDefault(hop, Collections.emptyList()))) {
            final String behind = owned.behind(hop);
            if (behind.isEmpty()) {
                break;
            }
            hop = behind;
            if (rows.containsKey(hop) && Behaved.written(hop)) {
                found = hop;
            }
        }
        return found;
    }

    private static boolean bare(final Collection<Map<String, String>> rows) {
        boolean found = true;
        for (final Map<String, String> row : rows) {
            final String name = row.getOrDefault("name", "");
            if (!name.isEmpty() && !"φ".equals(name)
                && !"true".equals(row.get("void")) && Behaved.open(name)) {
                found = false;
                break;
            }
        }
        return found;
    }

    private static boolean written(final String locator) {
        return Behaved.open(locator)
            && !locator.contains(".+") && !locator.contains(".-");
    }

    private static boolean open(final String name) {
        return !name.startsWith("+") && !name.startsWith("-") && !name.contains("🌵");
    }
}
