/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The rows about a type, with the name its behaviour goes by.
 *
 * <p>{@link Behaved} works out which types have no behaviour of their own and
 * what stands behind them; this writes the answer on the row, as one more cell
 * of the table the rules already keep about a type:</p>
 *
 * <pre> &lt;type id="Φ.bytes.as-bytes" complete="false" reduced="Φ.bytes"&gt;</pre>
 *
 * <p>It goes into the table rather than onto the drawn page alone, so that the
 * name a reader is given and the name the goal counts are one name. A cell and
 * not a pair in the links: saying that a {@code Φ.bytes.as-bytes} is a copy of
 * a {@code Φ.bytes} would put the fact where {@link Ends} and {@link Provided}
 * already look, which is why it was worth trying, and it corrupts what an
 * application fills — {@link Bound} settles the name of what a dispatch is
 * taken from before asking which void an argument lands in, so a
 * {@code plus 2} would fill a void of {@code Φ.number} instead of the
 * {@code x} of {@code Φ.number.plus}. A pair says the two objects are one, and
 * they are not: one of them behaves like the other, which is a weaker thing to
 * say and needs a weaker place to say it.</p>
 *
 * <p>It is read where an object is asked what it turns out to be, and only the
 * name travels. What is left to fill in stays as it was: the reduced name is
 * the formation the body arrived at, and the body filled that formation's
 * voids itself, inside itself, so counting them again against the new name
 * would leave a {@code Φ.true.+can-conjoin-true-with-true} that has nothing
 * left to fill described as still wanting an argument — 2,054 of the 4,469
 * renamed objects of eo-runtime report a different number if it is asked of
 * the new name.</p>
 *
 * @since 0.71.0
 */
public final class Reduced implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     *
     * @param clues The clues to follow before the types are reduced
     */
    public Reduced(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("provides.xml");
        final XML given = new XMLDocument(table);
        final Map<String, String> behaves = new Behaved(
            given,
            new Ends(new Pairs(new XMLDocument(tables.resolve("links.xml"))).all()).names()
        ).all();
        for (final XML row : given.nodes("/provides/type")) {
            final String found = behaves.get(new Noted(row).says("id"));
            if (found != null) {
                new Xembler(new Directives().attr("reduced", found))
                    .applyQuietly(row.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }
}
