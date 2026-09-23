/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import javax.xml.transform.stream.StreamSource;
import org.cactoos.iterable.Mapped;
import org.cactoos.iterable.Sorted;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The planting of the entries of the build.
 *
 * <p>A formation is folded by being evaluated, and evaluating it means
 * applying it to something. Its voids are not known at compile time, so
 * each of them is filled with a symbol, a lambda nobody answers, wrapped
 * in the carrier the tables of {@code eo:inference} name for it, and the
 * application is written down as one entry. What the evaluation comes
 * back with is the body of the formation written in terms of its own
 * inputs, which is exactly what a Java method needs to say.</p>
 *
 * <p>A symbol is never planted bare, since a formation holding nothing
 * but a lambda carries no attribute for the body to dispatch off. What
 * the tables cannot type is not planted at all: the body reaches the
 * bottom where it reads it, and the entry is a taint the run records and
 * the later stages leave alone.</p>
 *
 * <p>The whole build is read by one transformation, which is why this
 * class hands {@code entries.xsl} a manifest of the sources rather than a
 * source, and takes three files out of the one document that comes
 * back.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Plant a void of a void of an object. A void the
 *  tables type as an object other than a carrier is planted as that
 *  object applied to symbols for its own voids, and there the planting
 *  stops: a void of that object which is again such an object is left
 *  unfilled, and the entry is a taint for no better reason than the depth
 *  it stands at. Let {@code entries.xsl} go down as far as the types go,
 *  with a guard against a type that holds itself, and say in
 *  {@code voids.tsv} what it planted.
 */
final class Planting implements Stage {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory with the tables of {@code eo:inference}.
     */
    private final Path tables;

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory where the lowering keeps what it makes
     */
    Planting(final Collection<Path> srcs, final Path tbls, final Path dir) {
        this.sources = srcs;
        this.tables = tbls;
        this.home = dir;
    }

    @Override
    public void exec() throws IOException {
        if (!Files.exists(this.tables.resolve("provides.xml"))) {
            throw new IllegalStateException(
                String.format(
                    "There is no 'provides.xml' in '%s', while planting needs the tables of eo:inference to say what a void holds",
                    this.tables
                )
            );
        }
        final XML planted = new XSLDocument(
            Planting.class.getResource("/org/eolang/lowering/entries.xsl"),
            "/org/eolang/lowering/entries.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("inference", this.tables.toUri().toString())
            .transform(this.manifest());
        Files.createDirectories(this.home);
        this.save("entries.xmir", planted.nodes("/planted/object").get(0).toString());
        this.save("voids.tsv", String.join("", planted.xpath("/planted/voids/text()")));
        this.save("entries.tsv", String.join("", planted.xpath("/planted/entries/text()")));
        Logger.info(
            this,
            "Planted %s entries of %d files with %s symbols, %s voids left unfilled, into %[file]s",
            planted.xpath("/planted/@entries").get(0),
            this.sources.size(),
            planted.xpath("/planted/@symbols").get(0),
            planted.xpath("/planted/@unfilled").get(0),
            this.home
        );
    }

    private XML manifest() {
        final Directives dirs = new Directives().add("sources");
        for (final String uri
            : new Sorted<>(new Mapped<>(src -> src.toUri().toString(), this.sources))) {
            dirs.add("source").set(uri).up();
        }
        return new XMLDocument(new Xembler(dirs).xmlQuietly());
    }

    private void save(final String name, final String content) throws IOException {
        Files.write(
            this.home.resolve(name), content.getBytes(StandardCharsets.UTF_8)
        );
    }
}
