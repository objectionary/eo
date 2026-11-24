/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import io.github.sekator778.seqdir.Seqdir;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Mojo that gives the positional arguments of a program the names of the
 * voids they land in, right after the {@code inference} goal.
 *
 * <p>The parser has no way to know which void an argument fills, so it names
 * them by their places: in {@code foo k}, the {@code k} is {@code α0}. The
 * {@code answers.xml} of {@link MjInfer} knows better, since the row of
 * an argument names the void it lands in whenever there is one. This goal
 * reads it and writes {@code foo (bar: k)} instead, wherever the answer is
 * certain, as described in #8301:</p>
 *
 * <pre> &lt;o loc="Φ.app.φ.α0" copies="1" lands="bar"&gt;
 *   &lt;is loc="Φ.number"/&gt;
 * &lt;/o&gt;</pre>
 *
 * <p>The row says that {@code Φ.app.φ.α0} is {@code bar}. The engine writes
 * the name only when every application it saw puts the argument into one
 * void and nowhere else. An argument has no {@code lands} when it lands in
 * two voids, or when the engine cannot tell which formation the application
 * is made on, such as an unknown or an object made in Java.</p>
 *
 * <p>The engine sees only the applications written in the program, and that
 * is enough: EO always compiles the whole program, so there is no other
 * program that could attach something else to a void. When another program
 * appears, the compiler and the engine run again (#10056).</p>
 *
 * <p>An argument keeps its place where no single void is known to take it,
 * and the number of those is written to the log. With {@link #rigid} set,
 * the build fails when there is one.</p>
 *
 * <p>The XMIR goes to a directory {@code Seqdir} numbers "dealpha", and
 * {@link MjTranspile} reads it from there.</p>
 *
 * @since 0.69.0
 */
@Mojo(
    name = "dealpha",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjDealpha extends MjSafe {

    /**
     * The directory with the {@code answers.xml} that {@link MjInfer}
     * writes, falling back to a directory {@code Seqdir} numbers "inference"
     * when unset.
     */
    @Parameter(alias = "dealpha.tables", property = "eo.dealpha.tables")
    private File tables;

    /**
     * Whether to fail the build when a positional argument is left without
     * the name of its void.
     */
    @Parameter(
        alias = "dealpha.rigid",
        property = "eo.dealpha.rigid",
        defaultValue = "false"
    )
    private boolean rigid;

    /**
     * Ctor.
     */
    public MjDealpha() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path inference;
        if (this.tables == null) {
            inference = new Seqdir(this.target.toPath(), 2).dirs().once("inference");
        } else {
            inference = this.tables.toPath();
        }
        try (TjsForeign tojos = this.tojos()) {
            final Collection<TjForeign> objects = tojos.standalone();
            if (objects.isEmpty()) {
                Logger.debug(this, "No XMIR to name the arguments in");
            } else {
                new Dealphaing(
                    objects,
                    MjDealpha.names(inference.resolve("answers.xml")),
                    this.target.toPath(),
                    this.rigid
                ).exec();
            }
        }
    }

    private static Map<String, String> names(final Path answers) throws IOException {
        if (!Files.exists(answers)) {
            throw new IOException(
                Logger.format(
                    "The table %[file]s is absent, while the arguments can be named only after the 'infer' goal has written it",
                    answers
                )
            );
        }
        return new Xnav(new XMLDocument(answers).inner())
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
