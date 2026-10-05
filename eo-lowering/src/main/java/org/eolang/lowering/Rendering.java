/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import javax.xml.transform.stream.StreamSource;
import org.cactoos.Proc;
import org.cactoos.Text;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

/**
 * The stage that writes a Java atom for every entry that phino computed.
 *
 * <p>This is where all the work of the module pays off. Without this
 * stage, the body of an object is a graph of many small objects, which EO
 * builds and computes while the program runs. After this stage, the same
 * body is a few lines of Java: one line for every symbol that phino made
 * in the protocol. These lines are put into a Java class, which is an
 * atom. Later, {@link Patching} puts this atom into the EO object.</p>
 *
 * <p>The Java files are written into the directory of atoms, next to the
 * directory of protocols, so that a reader of the build can compare every
 * atom with the protocol it was made from. The Maven goal gives this
 * directory to javac as one more directory of sources. The file name of
 * every atom is exactly the class name that the transpiler will use when
 * it meets that atom, so javac finds it. The atoms are not written into
 * the directory of generated sources, because the transpiler deletes
 * every file there that it did not write itself.</p>
 *
 * <p>This stage reads the protocol, and not the result of phino. The
 * protocol already says which operations happened, in what order, and on
 * which symbols, and this is all that a Java method needs. To read the
 * result of phino, this module would have to parse a phi-expression, and
 * this module never does that. How a protocol becomes Java is decided
 * only by the stylesheet {@code rendering.xsl}, one protocol at a time.
 * This class only finds what the stylesheet needs to know about the
 * entry: its number, its locator, the top object it is inside, and the
 * package of that object.</p>
 *
 * <p>An entry that phino did not run has no protocol, so it is skipped.
 * When the stylesheet finds that an entry is a taint, this class only
 * writes into the log why, and the object stays in EO exactly as it was
 * written. Every skipped entry and every taint gets its own line in the
 * log, at the level INFO, so that a reader of the build sees why each
 * object was not turned into Java. At the end, this class writes the
 * list of the entries it turned into atoms into the file
 * {@code rendered.tsv}, so that {@link Patching} knows which objects to
 * change.</p>
 *
 * <p>The transpiler names the atom of an argument of another object after
 * the top object only, so two entries may ask for one class. Each of them
 * is a taint then, since javac would find only one of the two.</p>
 *
 * @since 0.64.0
 * @todo #8548:30min Write an atom for an entry whose result is always the
 *  same. When the result of the body is known bytes, like an object that
 *  always returns {@code 42}, the entry is a taint now. But its atom could
 *  simply return those bytes.
 * @todo #9222:30min Bump phino to the release that writes "of" and "with"
 *  in its deferred records, as asked in objectionary/phino#1732. The
 *  pinned 0.0.145 writes no deferred records at all, so no entry gets a
 *  deferred symbol yet. After the bump, check which entries of
 *  eo-runtime get deferred symbols, and that each of them is rendered or
 *  tainted as the tables of eo:inference say. The entry of
 *  "Φ.string.joined" stays a taint, since inference does not type what
 *  "with-delimiter" returns.
 */
final class Rendering implements Proc<Path> {

    /**
     * The directory where the Java atoms are written.
     */
    private final Path atoms;

    /**
     * The directory with the tables of {@code eo:inference}, which say what
     * an object copied by a deferred symbol returns.
     */
    private final Path tables;

    /**
     * Ctor.
     *
     * @param dir The directory where the Java atoms are written
     * @param tbls The directory with the tables of {@code eo:inference}
     */
    Rendering(final Path dir, final Path tbls) {
        this.atoms = dir;
        this.tables = tbls;
    }

    @Override
    public void exec(final Path home) throws IOException {
        final Path entries = home.resolve("entries.tsv");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while rendering needs the entries the planting writes",
                    entries
                )
            );
        }
        final Map<String, Path> tops = new HashMap<>(0);
        for (final Path copy : new Copies(home)) {
            tops.put(new XMLDocument(copy).xpath("/object/o[1]/@loc").get(0), copy);
        }
        final XSL sheet = new XSLDocument(
            Rendering.class.getResource("/org/eolang/lowering/rendering.xsl"),
            "/org/eolang/lowering/rendering.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("voids", home.resolve("voids.tsv").toUri().toString())
            .with("inference", this.tables.toUri().toString());
        final Collection<String> rendered = new ArrayList<>(0);
        final Map<String, Collection<String>> claims = new HashMap<>(0);
        int tainted = 0;
        for (final String row : new Filtered<>(
            line -> !line.isEmpty(),
            new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
        )) {
            final String[] cells = row.split("\t", -1);
            final Path protocol = home.resolve("2-protocols")
                .resolve(new Locator(cells[1]).protocol());
            if (Files.exists(protocol)) {
                final String top = Rendering.top(tops, cells[1]);
                final XML out = sheet
                    .with("number", cells[0])
                    .with("locator", cells[1])
                    .with("top", top)
                    .with("source", tops.get(top).toUri().toString())
                    .transform(new XMLDocument(protocol));
                if (out.nodes("/rendered/atom").isEmpty()) {
                    tainted += 1;
                    Logger.info(
                        this,
                        "The entry %s at %s gets no Java atom and stays in EO as written, because: %s",
                        cells[0], cells[1], out.xpath("/rendered/taint/text()").get(0)
                    );
                } else {
                    final Path file = this.atoms.resolve(
                        out.xpath("/rendered/atom/@file").get(0)
                    );
                    Files.createDirectories(file.getParent());
                    Files.write(
                        file,
                        out.xpath("/rendered/atom/text()").get(0).getBytes(StandardCharsets.UTF_8)
                    );
                    rendered.add(String.format("%s%n", row));
                    claims.computeIfAbsent(
                        out.xpath("/rendered/atom/@file").get(0), key -> new ArrayList<>(1)
                    ).add(String.format("%s%n", row));
                    Logger.debug(
                        this,
                        "Rendered the entry %s at %s into %[file]s (%[size]s), with voids read: %s, statements: %s, ifs: %s",
                        cells[0], cells[1], file, Files.size(file),
                        out.xpath("/rendered/atom/@voids").get(0),
                        out.xpath("/rendered/atom/@statements").get(0),
                        out.xpath("/rendered/atom/@branches").get(0)
                    );
                }
            } else {
                Logger.info(
                    this,
                    "The entry %s at %s gets no Java atom and stays in EO as written, because it has no protocol at %[file]s, since phino did not run it",
                    cells[0], cells[1], protocol
                );
            }
        }
        tainted += this.unshared(claims, rendered);
        Files.write(
            home.resolve("rendered.tsv"),
            String.join("", rendered).getBytes(StandardCharsets.UTF_8)
        );
        Logger.info(
            this,
            "Rendered %d atoms into %[file]s, while %d entries were taints",
            rendered.size(), this.atoms, tainted
        );
    }

    private int unshared(
        final Map<String, Collection<String>> claims, final Collection<String> rendered
    ) throws IOException {
        int dropped = 0;
        for (final Map.Entry<String, Collection<String>> claim : claims.entrySet()) {
            if (claim.getValue().size() > 1) {
                rendered.removeAll(claim.getValue());
                Files.delete(this.atoms.resolve(claim.getKey()));
                dropped += claim.getValue().size();
                Logger.info(
                    this,
                    "%d entries get no Java atom and stay in EO as written, because all of them ask for %s",
                    claim.getValue().size(), claim.getKey()
                );
            }
        }
        return dropped;
    }

    private static String top(final Map<String, Path> tops, final String locator) {
        String found = "";
        for (final String loc : tops.keySet()) {
            if ((locator.equals(loc) || locator.startsWith(String.format("%s.", loc)))
                && loc.length() > found.length()) {
                found = loc;
            }
        }
        if (found.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "The entry at %s is inside none of the copies, while its atom is named after the top object it lives in",
                    locator
                )
            );
        }
        return found;
    }
}
