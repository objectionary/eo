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
 * The rows about a void, with the object its callers settle it at.
 *
 * <p>A void row says what it holds only when the source wrote it down, as
 * {@code holds}, and everything the passes learn about the same void arrives
 * beside it as a census {@link Witnessed} writes. Of the 2,030 void rows of
 * eo-runtime 629 carry the annotation, and 810 of the rest are filled one way
 * and no other, so the answer is in the table and nobody has said it. This
 * says it, as one more cell of the row:</p>
 *
 * <pre> &lt;attr name="x" type="Φ.inc.x" void="true" settled="Φ.number"/&gt;</pre>
 *
 * <p>A cell of its own and not the {@code holds} the source writes, because the
 * two are different facts and one of them beats the other. What a source
 * declares is true of every caller there will ever be; what a census holds is
 * true of the callers this program happens to have, and {@link Answers} already
 * lets the annotation win where they disagree. Writing a sighting into the cell
 * that carries a declaration would put the two beyond telling apart, and the
 * readers that walk through a void on the strength of what it declares —
 * {@link Held} and {@link Provided} — would be walking on a sighting instead.
 * So a row the source typed is left as it stands.</p>
 *
 * <p>Nothing is worked out here that {@link Ones} did not work out already: the
 * census is read back, a census of one member that names a row of the table is
 * the answer, and a choice, a variable, an {@code unknown} and a member no
 * reader could go and look at leave the row alone. The point is where the
 * answer ends up. A reader of the table would otherwise redo the union of the
 * annotation, the census and the chase through the links to learn what a void
 * holds, and the first such reader is a stylesheet that must chase nothing.</p>
 *
 * @since 0.74.0
 */
public final class Told implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     *
     * @param clues The clues to follow before the rows are told
     */
    public Told(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("provides.xml");
        final XML given = new XMLDocument(table);
        final Map<String, String> ones = new Ones(given).all();
        for (final XML hollow : given.nodes("//attr[@void='true' and not(@holds)]")) {
            final String sole = ones.getOrDefault(new Noted(hollow).says("type"), "");
            if (!sole.isEmpty()) {
                new Xembler(new Directives().attr("settled", sole))
                    .applyQuietly(hollow.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }
}
