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
import org.cactoos.Proc;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The stage that writes down the entries of the build.
 *
 * <p>An "entry" is an object that may become an atom, together with made-up
 * inputs. To learn what an object computes, phino has to run it, and to run
 * it, phino needs values for its inputs. In EO, the inputs of an object are
 * called its "voids". Their real values are not known while the program is
 * compiled. So, every void gets a "symbol" instead of a value. A symbol is
 * a placeholder, a name that stands for a value nobody knows yet. phino then
 * computes the body of the object in terms of these symbols. The result
 * says, for example, "multiply the first input by two". This is exactly
 * what a Java method needs to know.</p>
 *
 * <p>A symbol is never given alone. It is wrapped into the type that the
 * stage {@code eo:inference} found for that void, such as a number or a
 * bool, because the body of the object needs the attributes of that type.
 * When {@code eo:inference} does not know the type of a void, that void
 * gets nothing at all. Then phino cannot finish the work on that entry,
 * and the entry becomes a "taint". A taint is an entry that cannot be
 * turned into Java, and the next stages simply leave it alone.</p>
 *
 * <p>What the body of an object gives is not asked here. An object gets
 * an entry whatever its body is, because the Java atom may give back the
 * result of the body as the object it is, for example a copy of another
 * object. Only {@link Rendering} asks {@code eo:inference} about the body,
 * and only when the result turns out to be plain data, which the atom
 * would give back without the object around it.</p>
 *
 * <p>All the sources of the build are handled by one XSL transformation,
 * {@code entries.xsl}. This is why this class gives the transformation a
 * list of all the sources, and not one source. The transformation returns
 * one document, and this class saves three files from it:
 * {@code entries.xmir}, {@code voids.tsv} and {@code entries.tsv}.</p>
 *
 * @since 0.64.0
 */
final class Planting implements Proc<Path> {

    /**
     * The directory with the tables of {@code eo:inference}, which say the
     * types of the voids.
     */
    private final Path tables;

    /**
     * Ctor.
     *
     * @param tbls The directory with the tables of {@code eo:inference}
     */
    Planting(final Path tbls) {
        this.tables = tbls;
    }

    @Override
    public void exec(final Path home) throws IOException {
        if (!Files.exists(this.tables.resolve("provides.xml"))) {
            throw new IllegalStateException(
                String.format(
                    "There is no 'provides.xml' in '%s', while planting needs the table of eo:inference to say what a void holds",
                    this.tables
                )
            );
        }
        final Collection<Path> sources = new ListOf<>(new Copies(home));
        final XML planted = new XSLDocument(
            Planting.class.getResource("/org/eolang/lowering/entries.xsl"),
            "/org/eolang/lowering/entries.xsl"
        ).with(new Hrefs())
            .with("inference", this.tables.toUri().toString())
            .transform(Planting.manifest(sources));
        Files.createDirectories(home);
        Planting.save(
            home.resolve("entries.xmir"), planted.nodes("/planted/object").get(0).toString()
        );
        Planting.save(
            home.resolve("voids.tsv"), String.join("", planted.xpath("/planted/voids/text()"))
        );
        Planting.save(
            home.resolve("entries.tsv"),
            String.join("", planted.xpath("/planted/entries/text()"))
        );
        Logger.info(
            this,
            String.join(
                "",
                "Planted %s entries of %d files with %s symbols, %s voids left unfilled, ",
                "into %[file]s, and left out %s atoms, %s formations without a body, ",
                "and %s formations under an argument without a name"
            ),
            planted.xpath("/planted/@entries").get(0),
            sources.size(),
            planted.xpath("/planted/@symbols").get(0),
            planted.xpath("/planted/@unfilled").get(0),
            home,
            planted.xpath("/planted/@atom").get(0),
            planted.xpath("/planted/@bodiless").get(0),
            planted.xpath("/planted/@placed").get(0)
        );
    }

    private static XML manifest(final Collection<Path> sources) {
        final Directives dirs = new Directives().add("sources");
        for (final String uri : new Mapped<>(src -> src.toUri().toString(), sources)) {
            dirs.add("source").set(uri).up();
        }
        return new XMLDocument(new Xembler(dirs).xmlQuietly());
    }

    private static void save(final Path file, final String content) throws IOException {
        Files.write(file, content.getBytes(StandardCharsets.UTF_8));
    }
}
