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
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.cactoos.Proc;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
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
 * The type is the annotation of the void in {@code facts.xml}, when the
 * source wrote one, and otherwise the only object {@code answers.xml} names
 * for the void. So an object gets an entry only when every void of it
 * holds a number, a string, a bool, bytes or a tuple. When a void holds
 * any other object, or when {@code eo:inference} does not know what it
 * holds, the object gets no entry at all and stays in EO as it was
 * written. A void left empty would not stop phino: it drops the part of
 * the body that reads that void, and the Java atom would quietly lose that
 * part.</p>
 *
 * <p>An object gets an entry whatever its body gives, because the Java
 * atom may give back the result of the body as the object it is, for
 * example a copy of another object. Only {@link Rendering} asks
 * {@code eo:inference} about the body when the result turns out to be
 * plain data, which the atom would give back without the object around
 * it.</p>
 *
 * <p>This stage asks {@code eo:inference} about the body for one thing
 * only: whether the object becomes a "row" of the table of operations,
 * {@code atoms.yaml}. Without rows, phino folds the whole tree of calls
 * under a body into one atom, and the atom of {@code floor} computes
 * {@code is-finite} inside itself. So every object whose body gives a
 * number, a string, a bool or bytes, and whose voids all hold such data,
 * gets a row, and every reference to that object makes phino fire the row
 * instead of folding the body: the row dataizes the inputs of the object
 * once and answers a fresh symbol wrapped in the type the body gives. In
 * the copy of the source, the body of such an object moves under the name
 * {@code body🌵} and a λ named after the object stands beside it, so
 * phino takes the object for an atom, while the entry of the object itself
 * reaches its body by that name and still spells it out. The rows are
 * written into {@code rows.tsv} for {@link Rendering}, which turns every
 * symbol such a λ made into a call of the object in Java, and into
 * {@code atoms.yaml}, after the rows of the atoms of the library, for
 * {@link Morphing}.</p>
 *
 * <p>All the sources of the build are handled by one XSL transformation,
 * {@code entries.xsl}. This is why this class gives the transformation a
 * list of all the sources, and not one source. The transformation returns
 * one document, and this class saves the files from it:
 * {@code entries.xmir}, {@code voids.tsv}, {@code entries.tsv},
 * {@code rows.tsv}, {@code atoms.yaml}, and every copy of a source that
 * holds an object with a row, written over the copy it was made from.</p>
 *
 * @since 0.64.0
 */
final class Planting implements Proc<Path> {

    /**
     * The directory with the tables of {@code eo:inference},
     * {@code facts.xml} and {@code answers.xml}, which say what the voids
     * hold and what the bodies give.
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
        for (final String table : new ListOf<>("facts.xml", "answers.xml")) {
            if (!Files.exists(this.tables.resolve(table))) {
                throw new IllegalStateException(
                    String.format(
                        "There is no '%s' in '%s', while planting needs the tables of eo:inference to say what a void holds and what a body gives",
                        table, this.tables
                    )
                );
            }
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
        Planting.save(
            home.resolve("rows.tsv"), String.join("", planted.xpath("/planted/rows/text()"))
        );
        Planting.save(
            home.resolve("atoms.yaml"),
            String.join(
                "",
                new IoCheckedText(
                    new TextOf(new ResourceOf("org/eolang/lowering/atoms.yaml"))
                ).asString(),
                String.join("", planted.xpath("/planted/table/text()"))
            )
        );
        for (final XML copy : planted.nodes("/planted/copies/copy")) {
            Planting.save(
                Paths.get(URI.create(copy.xpath("@href").get(0))),
                copy.nodes("object").get(0).toString()
            );
        }
        Logger.info(
            this,
            String.join(
                "",
                "Planted %s entries of %d files with %s symbols into %[file]s, ",
                "gave %s of them a row of the table of operations, ",
                "and left out %s atoms, %s formations without a body, ",
                "%s formations under an argument without a name, ",
                "and %s formations with a void that holds no number, string, bool, bytes or tuple"
            ),
            planted.xpath("/planted/@entries").get(0),
            sources.size(),
            planted.xpath("/planted/@symbols").get(0),
            home,
            planted.xpath("/planted/@rows").get(0),
            planted.xpath("/planted/@atom").get(0),
            planted.xpath("/planted/@bodiless").get(0),
            planted.xpath("/planted/@placed").get(0),
            planted.xpath("/planted/@opaque").get(0)
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
