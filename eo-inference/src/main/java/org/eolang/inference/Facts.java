/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Everything the program states about itself, in one document.
 *
 * <p>The facts are what the text says before anything is worked out, and
 * they come in kinds, a {@link Section} each. This clue asks every section
 * for its part and writes them all into {@code facts.xml}, in the order it
 * was given them, under one root:</p>
 *
 * <pre> &lt;facts&gt;
 *   &lt;objects&gt;…&lt;/objects&gt;
 *   &lt;reads&gt;…&lt;/reads&gt;
 *   &lt;heads&gt;…&lt;/heads&gt;
 *   &lt;calls&gt;…&lt;/calls&gt;
 * &lt;/facts&gt;</pre>
 *
 * <p>Each run writes the four sections into {@code facts.xml}. The
 * {@link Walk} engine reads that document to work out answers for each
 * place; those answers are written separately into {@code answers.xml}.
 * The engine does not add its answers to the facts.</p>
 *
 * <p>Every section reads the program as text, writing a row for an object
 * because it is <em>there</em>, not because something reaches it. The
 * alternative — starting from a root and following what actually runs —
 * gives sharper answers, since every question is then asked in a context
 * where the types are concrete, but it cannot be used here: eo-runtime is
 * a library with no entry point, so a checker that waits to be reached
 * has nothing to pull on and reports a clean bill of health for code it
 * never opened. The engine studies formations with open voids as well as
 * the copies made by calls, resolving each place in the context of its
 * enclosing copy.</p>
 *
 * <p>No section decides anything, which is why nothing here says whether
 * a program is wrong. Judging used to live beside these rules and was
 * taken out again in #6661: it reported nothing, because a verdict needs
 * the object that misses an attribute to have been seen whole, and almost
 * none of them have been. It comes back when every object can be given a
 * type, rather than only those a call site happens to reach.</p>
 *
 * @since 0.76.0
 */
public final class Facts implements Clue {

    /**
     * The sections, in the order they are written.
     */
    private final Collection<Section> sections;

    /**
     * Ctor.
     */
    public Facts() {
        this(Arrays.asList(new Provides(), new Needs(), new Starts(), new Applications()));
    }

    /**
     * Ctor.
     *
     * @param parts The sections, in the order they are written
     */
    Facts(final Collection<Section> parts) {
        this.sections = parts;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Directives dirs = new Directives().add("facts");
        for (final Section section : this.sections) {
            dirs.append(section.directives(xmirs));
        }
        Files.createDirectories(tables);
        Files.write(
            tables.resolve("facts.xml"),
            new XMLDocument(new Xembler(dirs).domQuietly()).toString()
                .getBytes(StandardCharsets.UTF_8)
        );
    }
}
