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
 * @since 0.69.0
 */
final class Landings {

    /**
     * The path of {@code answers.xml}.
     */
    private final Path answers;

    /**
     * Ctor.
     *
     * @param path The path of {@code answers.xml}
     */
    Landings(final Path path) {
        this.answers = path;
    }

    /**
     * The name of the void of every argument that lands in exactly one.
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
        return new Xnav(new XMLDocument(this.answers).inner())
            .element("answers")
            .elements(Filter.withName("o"))
            .filter(row -> row.attribute("lands").text().isPresent()).collect(
                Collectors.toMap(
                    row -> row.attribute("loc").text().get(),
                    row -> row.attribute("lands").text().get()
                )
            );
    }
}
