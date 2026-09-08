/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What the tables say about every object of the program.
 *
 * <p>The tables are asked once, here, and both the ones who want the answers
 * read the same map: the goal that counts how much of the program we
 * understand, and the page that shows somebody which parts of their file we
 * do not. Asking twice would let the two drift, and a page that draws an
 * object green while the printed number counts it among the ones we know
 * nothing about is worse than either of them alone.</p>
 *
 * <p>What an atom comes back with is asked for along with the rest. The body
 * of an atom is a {@code λ} nobody types and nothing here can read, so it is
 * a copy of nothing, has no row of its own and no void above it, and comes out
 * as an object we know nothing about — which was every atom of the program
 * until {@link Returned} was asked here as well as inside the walk (#8317).
 * The source said all along what running the body gives back, and now the body
 * answers with it.</p>
 *
 * <p>Which voids an atom fills is stamped on afterwards, by {@link Forged},
 * rather than worked out inside the walk. It is not something the walk found
 * out — it is the same name rooted at the same void, and only the reason it
 * stayed there differs — and the walk has no business carrying a fact it never
 * uses (#8352).</p>
 *
 * @since 0.70.0
 */
final class Answered {

    /**
     * The directory with the prepared XMIR files.
     */
    private final Path world;

    /**
     * The directory with the tables.
     */
    private final Path tables;

    /**
     * Ctor.
     * @param xmirs The directory with the prepared XMIR files
     * @param rows The directory with the tables
     */
    Answered(final Path xmirs, final Path rows) {
        this.world = xmirs;
        this.tables = rows;
    }

    /**
     * The answer for every object of the program.
     * @return The answers, by the locator of the object
     * @throws IOException If a table or a file cannot be read
     */
    Map<String, Answer> all() throws IOException {
        final XML given = new XMLDocument(this.tables.resolve("provides.xml"));
        final Pairs pairs = new Pairs(new XMLDocument(this.tables.resolve("links.xml")));
        final Map<String, String> ends = new LinkedHashMap<>(new Ends(pairs.all()).names());
        ends.putAll(new Returned(given).bodies());
        final Answers answers = new Answers(
            new Ungrouped(given, Collections.emptyMap()).rows(),
            new Seen(given).all(),
            new HashSet<>(pairs.certain()),
            ends
        );
        final Map<String, Collection<String>> filled = pairs.filled();
        final Map<String, Answer> found = new LinkedHashMap<>(0);
        for (final String locator : new Xmirs(this.world).locators()) {
            found.put(
                locator, answers.of(locator, filled.getOrDefault(locator, Collections.emptyList()))
            );
        }
        return new Forged(given).marked(found);
    }
}
