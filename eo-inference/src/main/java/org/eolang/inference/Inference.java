/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.parser.TrFull;

/**
 * The types of the objects of a program.
 *
 * <p>A program is a directory of XMIR files, because an object in one
 * file is used by objects in others and no single file can be typed on
 * its own. Give it that directory and it writes down everything it can
 * work out:</p>
 *
 * <pre> new Inference(input).inferTo(output, tables);</pre>
 *
 * <p>In {@code output} the XMIR files are found again, prepared for the
 * rules; in {@code tables} the tables of what has been worked out.</p>
 *
 * <p>The checker looks for one kind of mistake: an attribute taken from
 * an object that certainly doesn't have it. Every object keeps the type
 * it was born with — nothing is ever renamed or merged — and what we
 * learn about those types goes into tables instead, one row at a time.
 * A type is named by the locator the parser gives every object,
 * {@code Φ.app.inc.φ.ρ}, which is unique across the whole program and
 * needs no dictionary to be read: a row saying that {@code Φ.app.t.next}
 * has nothing means something to a human as it stands. It also makes the
 * tables of many files one table, since no two files can name the same
 * locator.</p>
 *
 * <p>Two things happen to a file before any rule looks at it. First,
 * every composite base is split into one object per dispatch step: the
 * parser rolls {@code x.next.foo} into a single base, while a rule can
 * only notice an attribute being taken when the taking is an object of
 * its own. Second, the locators are set again, because the objects that
 * the splitting has just created have none. That is what {@code output}
 * holds, and it is worth keeping: every row of every table points into
 * those files.</p>
 *
 * @since 0.67.0
 */
public final class Inference {

    /**
     * The directory with the XMIR files of the program.
     */
    private final Path input;

    /**
     * Ctor.
     * @param dir The directory with XMIR files, as the parser leaves them
     *  after its canonical pipeline (see
     *  {@code org.eolang.parser.Canonical})
     */
    public Inference(final Path dir) {
        this.input = dir;
    }

    /**
     * Work out the types and write down what is known.
     * @param output The empty directory for the prepared XMIR files,
     *  which keep the names they had; nothing is deleted from it, so a
     *  file left there by an earlier run would outlive its source
     * @param tables The directory for the tables, a document each
     * @throws IOException If a file cannot be read or written
     */
    public void inferTo(final Path output, final Path tables) throws IOException {
        final Xsline train = new Xsline(
            new TrFull(
                new TrDefault<>(
                    new StClasspath("/org/eolang/inference/unroll-bases.xsl"),
                    new StClasspath("/org/eolang/parser/parse/set-locators.xsl")
                )
            )
        );
        final Collection<XML> prepared = new ArrayList<>(0);
        for (final Path source : this.sources()) {
            final XML ready = train.pass(new XMLDocument(source));
            final Path target = output.resolve(this.input.relativize(source));
            Files.createDirectories(target.getParent());
            Files.write(target, ready.toString().getBytes(StandardCharsets.UTF_8));
            prepared.add(ready);
        }
        Files.createDirectories(tables);
        Files.write(
            tables.resolve("provides.xml"),
            new Grouped(new Provides(prepared).rows(), "provides")
                .asXml()
                .toString()
                .getBytes(StandardCharsets.UTF_8)
        );
    }

    /**
     * Every XMIR file of the program, in the same order every time, so
     * that the tables come out the same every time too.
     * @return The files
     * @throws IOException If the directory cannot be walked
     */
    private Collection<Path> sources() throws IOException {
        try (Stream<Path> found = Files.walk(this.input)) {
            return found
                .filter(path -> path.toString().endsWith(".xmir"))
                .sorted()
                .collect(Collectors.toList());
        }
    }
}
