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
 * <p>A choice longer than the cap is written as {@code unknown} instead of its
 * members. {@code Φ.tuple.head} is filled with 56 different types, and a
 * choice of 56 tells a reader nothing except that nobody has thought about
 * it; saying so outright is shorter and truer. Eleven voids of eo-runtime are
 * over the cap, and every one of them holds whatever it is handed: the target
 * of a {@code dataized}, the scope of a {@code malloc}, the body of a
 * {@code while}.</p>
 *
 * @since 0.69.0
 */
public final class Witnessed implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * How many members a choice may have before it says nothing.
     */
    private final int cap;

    /**
     * Ctor.
     * @param clues The clues to follow before the voids are looked into
     */
    public Witnessed(final Clue clues) {
        this(clues, 8);
    }

    /**
     * Ctor.
     * @param clues The clues to follow before the voids are looked into
     * @param members How many members a choice may have before it says
     *  nothing, measured at eight in the state document
     */
    Witnessed(final Clue clues, final int members) {
        this.origin = clues;
        this.cap = members;
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
                        .append(this.joined(members).directives())
                        .up()
                ).applyQuietly(hollow.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }

    private Type joined(final Collection<Type> members) {
        final Type found;
        if (members.size() > this.cap) {
            found = new Unknown();
        } else if (members.size() == 1) {
            found = members.iterator().next();
        } else {
            found = new Union(members);
        }
        return found;
    }
}
