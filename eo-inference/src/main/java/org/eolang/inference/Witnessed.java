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
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The rows about a void, with what the program is seen to put in it.
 *
 * <p>{@link Demanded} writes down what a void will have to offer. This writes
 * the other half of what is known about one — what actually goes in, gathered
 * by {@link Fillings} from every application that fills it, as the choice
 * between the types they put there:</p>
 *
 * <pre> &lt;attr name="φ" type="Φ.bytes.φ" void="true"&gt;
 *   &lt;witnessed&gt;
 *     &lt;union&gt;
 *       &lt;data/&gt;
 *       &lt;ref loc="Φ.bytes.as-bytes"/&gt;
 *     &lt;/union&gt;
 *   &lt;/witnessed&gt;
 * &lt;/attr&gt;</pre>
 *
 * <p>Where the choice has one member, that member is the type of the void, and
 * {@link Answers} says so. There is no tomorrow for such a claim to leak into:
 * a build parses the library it uses along with the program, transpiles it
 * again, and keys the cache on the rows it wrote, so a caller who passes
 * something else is in a run of their own, where the void has two witnesses
 * and is a void again. What a program does with a void everywhere is a fact
 * about that program, and refusing to read it is refusing to know it.</p>
 *
 * <p>A choice of several stays a choice. {@code Φ.bool.and.x} is filled with a
 * {@code Φ.true}, with a {@code Φ.false} and with five other things, and
 * naming any one of them would be picking a favourite among facts.</p>
 *
 * <p>Not every filling is an application. An atom calls what it is handed, and
 * a formation only Java ever copies is filled where no source can be read, so
 * the voids of one are answered by the annotation the atom carries and by
 * {@link Handed}, which reads it (#8380).</p>
 *
 * <p>A choice is written whole, however long it grows. {@code Φ.tuple.head}
 * is filled with 56 different types and {@code Φ.string.φ} with 26, and a
 * choice that long tells a reader nothing except that nobody has thought
 * about it, so the page says as much instead of listing it. But the page is
 * the only reader for whom the length is the point: {@link Seen} reads the
 * census back to type the voids with, and a census cut to {@code unknown}
 * where it was written told it nothing about a void the tables had already
 * worked out (#8844).</p>
 *
 * @since 0.69.0
 */
public final class Witnessed implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     *
     * @param clues The clues to follow before the voids are looked into
     */
    public Witnessed(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("provides.xml");
        final XML given = new XMLDocument(table);
        final Map<String, Collection<Type>> filled = new Fillings(
            new XMLDocument(tables.resolve("links.xml")), given
        ).all();
        for (final XML hollow : given.nodes("//attr[@void='true']")) {
            final Collection<Type> members = filled.getOrDefault(
                new Noted(hollow).says("type"), Collections.emptyList()
            );
            if (!members.isEmpty()) {
                new Xembler(
                    new Directives()
                        .add("witnessed")
                        .append(Witnessed.joined(members).directives())
                        .up()
                ).applyQuietly(hollow.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }

    private static Type joined(final Collection<Type> members) {
        final Type found;
        if (members.size() == 1) {
            found = members.iterator().next();
        } else {
            found = new Union(members);
        }
        return found;
    }
}
