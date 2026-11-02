/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The names of the voids that positional arguments land in.
 *
 * <p>Every place of the program is a row of {@code answers.xml}, and the row
 * of an argument names, in {@code lands}, the void that argument fills:</p>
 *
 * <pre> &lt;o loc="Φ.app.φ.α0" copies="1" lands="bar"&gt;
 *   &lt;is loc="Φ.number"/&gt;
 * &lt;/o&gt;</pre>
 *
 * <p>The row says that {@code Φ.app.φ.α0} is {@code bar}. The engine writes
 * the name only when the call puts the argument into one void and nowhere
 * else. An argument has no {@code lands}, and keeps its place, when it lands
 * in two voids, when a head of its call is something the engine cannot follow
 * to a formation, such as an open void or an object made in Java, or when
 * every head of its call fails.</p>
 *
 * <p>The engine finds that void from what the program was seen to put into
 * the head of the call, and a caller outside the program may put something
 * else there. In {@code cant-connect "failed"}, with {@code cant-connect} a
 * void, the string lands in {@code message} because every caller in the
 * program puts a formation with that void there, while a caller in Java may
 * put an object with no such void. So an argument is named only when
 * {@link Grounded} says the head of its call is fixed by the code alone,
 * followed in {@code facts.xml} without reading any void but {@code ρ}. Any
 * other argument keeps its place.</p>
 *
 * @since 0.69.0
 */
final class Landings {

    /**
     * The path of {@code answers.xml}.
     */
    private final Path answers;

    /**
     * The arguments whose call has a head the code alone fixes.
     */
    private final Grounded grounded;

    /**
     * Ctor.
     *
     * @param tables The directory with {@code answers.xml} and
     *  {@code facts.xml}
     */
    Landings(final Path tables) {
        this(tables.resolve("answers.xml"), new Grounded(tables.resolve("facts.xml")));
    }

    /**
     * Ctor.
     *
     * @param path The path of {@code answers.xml}
     * @param sure The arguments whose call has a head the code alone fixes
     */
    Landings(final Path path, final Grounded sure) {
        this.answers = path;
        this.grounded = sure;
    }

    /**
     * The name of the void of every argument that lands in exactly one, of
     * a call whose head the code alone fixes.
     *
     * @return The names, by the locator of the argument
     * @throws IOException If the table can't be read
     */
    Map<String, String> names() throws IOException {
        if (!Files.exists(this.answers)) {
            throw new IOException(
                Logger.format(
                    "The table %[file]s is absent, while the arguments can be named only after the 'inference' goal has written it",
                    this.answers
                )
            );
        }
        final Collection<String> sure = this.grounded.arguments();
        return new Xnav(new XMLDocument(this.answers).inner())
            .element("answers")
            .elements(Filter.withName("o"))
            .filter(row -> row.attribute("lands").text().isPresent())
            .filter(row -> sure.contains(row.attribute("loc").text().get())).collect(
                Collectors.toMap(
                    row -> row.attribute("loc").text().get(),
                    row -> row.attribute("lands").text().get()
                )
            );
    }
}
