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
import org.eolang.inference.Annotation;

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
 * {@code Φ.posix.status-return(Φ.number,Φ.string)}. The formation it fixes is
 * the name before the brackets, and the brackets fix what Java puts into the
 * voids of that formation. A call written in the program fixes the voids of
 * the copy it makes in the same way, with the arguments written there. So
 * {@link Calls} gets each type whole, the voids of every formation in the
 * order they are written, and the arguments of every call, split into those
 * passed by position and those bound by name.</p>
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
            new Calls(
                table.element("objects").elements(Filter.withName("type")).collect(
                    Collectors.toMap(
                        type -> type.attribute("id").text().get(),
                        type -> type.elements(Filter.withName("attr")).filter(
                            attr -> "true".equals(attr.attribute("void").text().orElse(""))
                        ).map(attr -> attr.attribute("name").text().get()).filter(
                            name -> !"ρ".equals(name)
                        ).collect(Collectors.toList()),
                        (first, second) -> first
                    )
                ),
                table.element("calls").elements(Filter.withName("call")).collect(
                    Collectors.toMap(
                        call -> call.attribute("loc").text().get(),
                        call -> call.elements(Filter.withName("arg")).filter(
                            arg -> arg.attribute("n").text().isPresent()
                        ).collect(
                            Collectors.toMap(
                                arg -> Integer.parseInt(arg.attribute("n").text().get()),
                                arg -> arg.attribute("loc").text().get(),
                                (first, second) -> first
                            )
                        ),
                        (first, second) -> first
                    )
                ),
                table.element("calls").elements(Filter.withName("call")).collect(
                    Collectors.toMap(
                        call -> call.attribute("loc").text().get(),
                        call -> call.elements(Filter.withName("arg")).filter(
                            arg -> arg.attribute("n").text().isEmpty()
                        ).collect(
                            Collectors.toMap(
                                arg -> arg.attribute("name").text().get(),
                                arg -> arg.attribute("loc").text().get(),
                                (first, second) -> first
                            )
                        ),
                        (first, second) -> first
                    )
                ),
                table.element("objects").elements(Filter.withName("type")).filter(
                    type -> type.attribute("returns").text().isPresent()
                ).collect(
                    Collectors.toMap(
                        type -> type.attribute("id").text().get(),
                        type -> new Annotation(type.attribute("returns").text().get()),
                        (first, second) -> first
                    )
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
        return type.elements(Filter.withName("attr")).collect(
            Collectors.toMap(
                attr -> attr.attribute("name").text().get(),
                attr -> attr.attribute("rho").text().orElseGet(
                    () -> attr.attribute("type").text().get()
                ),
                (first, second) -> second
            )
        );
    }
}
