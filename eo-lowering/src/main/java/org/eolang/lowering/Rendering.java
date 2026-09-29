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
 * The writing of the Java the folded formations became.
 *
 * <p>This is where the work of the pipeline is paid back. A body that was
 * an object graph built and dataized at runtime is a handful of Java
 * statements here, one per symbol the protocol minted, and the atom the
 * patch put into the formation is the class those statements live in. The
 * class is written into the directory of generated sources, where the
 * transpiler writes the classes of the build, so javac finds it under the
 * very name the transpiler gives the atom.</p>
 *
 * <p>The protocol is read and the program phino morphed is not, because
 * the protocol already says what fired, in what order, and off which
 * symbol, which is all a Java method is; reading the morphed program back
 * would mean parsing a phi-expression, and this module parses none. What a
 * protocol becomes is said by {@code rendering.xsl} alone, one protocol at a
 * time, and this stage only finds what that stylesheet needs to know about
 * the entry: its number, its locator, the top object it lives in, and the
 * package of that object. An entry whose run was killed has no protocol and
 * is skipped, and an entry the stylesheet finds a taint in is only
 * logged, so its formation stays in EO exactly as it was written.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Fall back to the atom where a slice is out of bounds.
 *  The slice of {@code rendering.xsl} throws when its range is outside
 *  the bytes, while the atom of {@code bytes.slice} answers with its
 *  {@code cant-slice} error, which EO code may catch. Render the slice so
 *  that it fails the way the atom does, or leave an entry that slices
 *  as a taint.
 * @todo #8548:60min Render the entries of formations that are arguments of
 *  an application. A locator with a {@code φ}, {@code ρ} or {@code α} step,
 *  like {@code Φ.true.φ.α0}, names a formation with no name of its own,
 *  whose atom the transpiler names by a rule {@code rendering.xsl} does
 *  not mirror, so such an entry is a taint now: four of the 587 entries of
 *  eo-runtime are.
 * @todo #8548:30min Render an entry whose root is a constant. A body that
 *  comes to known bytes, like a formation that always answers {@code 42},
 *  is a taint now, while its atom could return those bytes as they are.
 */
final class Rendering implements Proc<Path> {

    /**
     * The directory of generated sources the classes are written into.
     */
    private final Path generated;

    /**
     * Ctor.
     *
     * @param dir The directory of generated sources
     */
    Rendering(final Path dir) {
        this.generated = dir;
    }

    @Override
    public void exec(final Path target) throws IOException {
        final Path home = target.resolve("7-lowering");
        final Path entries = home.resolve("entries.tsv");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while rendering needs the entries the planting writes",
                    entries
                )
            );
        }
        final Map<String, String> tops = new HashMap<>(0);
        for (final Path copy : new Copies(target)) {
            final XML xmir = new XMLDocument(copy);
            tops.put(
                xmir.xpath("/object/o[1]/@loc").get(0),
                String.join("", xmir.xpath("/object/metas/meta[head='package']/tail/text()"))
            );
        }
        final XSL sheet = new XSLDocument(
            Rendering.class.getResource("/org/eolang/lowering/rendering.xsl"),
            "/org/eolang/lowering/rendering.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("voids", home.resolve("voids.tsv").toUri().toString());
        int rendered = 0;
        int tainted = 0;
        for (final String row : new Filtered<>(
            line -> !line.isEmpty(),
            new Mapped<>(Text::asString, new Split(new TextOf(entries), "\\R"))
        )) {
            final String[] cells = row.split("\t", -1);
            final Path protocol = target.resolve("7-lowering-protocols")
                .resolve(new Locator(cells[1]).protocol());
            if (Files.exists(protocol)) {
                final String top = Rendering.top(tops, cells[1]);
                final XML out = sheet
                    .with("number", cells[0])
                    .with("locator", cells[1])
                    .with("top", top)
                    .with("package", tops.get(top))
                    .transform(new XMLDocument(protocol));
                if (out.nodes("/rendered/atom").isEmpty()) {
                    tainted += 1;
                    Logger.debug(
                        this,
                        "The entry %s at %s is a taint: %s",
                        cells[0], cells[1], out.xpath("/rendered/taint/text()").get(0)
                    );
                } else {
                    final Path file = this.generated.resolve(
                        out.xpath("/rendered/atom/@file").get(0)
                    );
                    Files.createDirectories(file.getParent());
                    Files.write(
                        file,
                        out.xpath("/rendered/atom/text()").get(0).getBytes(StandardCharsets.UTF_8)
                    );
                    rendered += 1;
                    Logger.info(
                        this,
                        "Rendered the entry %s at %s into %[file]s (%[size]s), with voids read: %s, statements: %s, ifs: %s",
                        cells[0], cells[1], file, Files.size(file),
                        out.xpath("/rendered/atom/@voids").get(0),
                        out.xpath("/rendered/atom/@statements").get(0),
                        out.xpath("/rendered/atom/@branches").get(0)
                    );
                }
            }
        }
        Logger.info(
            this,
            "Rendered %d atoms into %[file]s, while %d entries were taints",
            rendered, this.generated, tainted
        );
    }

    private static String top(final Map<String, String> tops, final String locator) {
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
