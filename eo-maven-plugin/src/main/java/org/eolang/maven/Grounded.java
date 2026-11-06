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
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The arguments of the calls whose head the code alone fixes.
 *
 * <p>Which void an argument lands in depends on what its call copies, and
 * the engine answers that from what the program was seen to put there. In
 * this program</p>
 *
 * <pre> [fail] &gt; guard
 *   fail &gt; @
 *     "it broke"</pre>
 *
 * <p>every caller puts into {@code fail} a formation whose void is
 * {@code message}, so the engine says the string lands in {@code message}.
 * It knows only the callers it saw: a caller in Java, or in a program built
 * later, may put there a formation whose void has another name, and an
 * argument passed by name then finds no void to fill. An argument is
 * grounded only when {@link Origins} follows the head of its call to one
 * formation from {@code facts.xml} alone, since then no caller can change the
 * void it lands in.</p>
 *
 * <p>What an atom returns is a type, such as
 * {@code Φ.posix.status-return(Φ.number,Φ.string)}, and the formation it
 * fixes is the name before the brackets. A union, such as
 * {@code Φ.true|Φ.false}, keeps the name of each side, and {@link Origins}
 * follows a call on it only when both sides lead the call to the same
 * formation.</p>
 *
 * @since 0.76.0
 */
final class Grounded {

    /**
     * The path of {@code facts.xml}.
     */
    private final Path facts;

    /**
     * Ctor.
     *
     * @param path The path of {@code facts.xml}
     */
    Grounded(final Path path) {
        this.facts = path;
    }

    /**
     * The locators of the arguments whose call has a head the code fixes.
     *
     * @return The locators
     * @throws IOException If the table can't be read
     */
    Collection<String> arguments() throws IOException {
        if (!Files.exists(this.facts)) {
            throw new IOException(
                Logger.format(
                    "The table %[file]s is absent, while the heads of calls can be followed only after the 'inference' goal has written it",
                    this.facts
                )
            );
        }
        final Xnav table = new Xnav(new XMLDocument(this.facts).inner()).element("facts");
        final Origins origins = new Origins(
            table.element("objects").elements(Filter.withName("type")).collect(
                Collectors.toMap(
                    type -> type.attribute("id").text().get(),
                    Grounded::targets,
                    (first, second) -> first
                )
            ),
            table.element("reads").elements(Filter.withName("read")).collect(
                Collectors.toMap(
                    read -> read.attribute("id").text().get(),
                    read -> Map.entry(
                        read.attribute("of").text().get(),
                        read.attribute("name").text().get()
                    ),
                    (first, second) -> first
                )
            ),
            table.element("heads").elements(Filter.withName("type")).filter(
                type -> type.elements(Filter.withName("ref")).findAny().isPresent()
            ).collect(
                Collectors.toMap(
                    type -> type.attribute("id").text().get(),
                    type -> type.element("ref").attribute("loc").text().get(),
                    (first, second) -> first
                )
            )
        );
        return table.element("calls").elements(Filter.withName("call"))
            .filter(call -> origins.formation(call.attribute("loc").text().get()).isPresent())
            .flatMap(call -> call.elements(Filter.withName("arg")))
            .map(arg -> arg.attribute("loc").text().get())
            .collect(Collectors.toSet());
    }

    private static Map<String, String> targets(final Xnav type) {
        final Map<String, String> attrs = new HashMap<>(0);
        type.attribute("returns").text().ifPresent(back -> attrs.put("φ", Grounded.head(back)));
        type.elements(Filter.withName("attr")).forEach(
            attr -> attrs.put(attr.attribute("name").text().get(), Grounded.target(attr))
        );
        return attrs;
    }

    private static String head(final String type) {
        final StringBuilder outside = new StringBuilder(type.length());
        int depth = 0;
        for (int idx = 0; idx < type.length(); ++idx) {
            final char glyph = type.charAt(idx);
            if (glyph == '(') {
                depth += 1;
            } else if (glyph == ')') {
                depth -= 1;
            } else if (depth == 0) {
                outside.append(glyph);
            }
        }
        return outside.toString();
    }

    private static String target(final Xnav attr) {
        final String found;
        if ("true".equals(attr.attribute("void").text().orElse(""))) {
            found = attr.attribute("rho").text().orElse("");
        } else {
            found = attr.attribute("type").text().get();
        }
        return found;
    }
}
