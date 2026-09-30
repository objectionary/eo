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
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import org.cactoos.Proc;
import org.cactoos.iterable.Sorted;

/**
 * The stage that puts the atoms into the EO objects.
 *
 * <p>This is the last stage. It must come after {@link Rendering}, because
 * the body of an object may be taken away only when {@link Rendering}
 * really wrote a Java atom for it. An object whose entry is a taint, or
 * whose run was stopped because of the time limit, keeps its body, and
 * the transpiler compiles it as usual.</p>
 *
 * <p>This stage reads two things: the XMIR files of the build, with their
 * tests, and the file {@code rendered.tsv}, which lists the entries that
 * {@link Rendering} turned into atoms. It never reads the result of phino.
 * The change of one file is done by the stylesheet
 * {@code patching.xsl}. In an object whose entry was rendered, the
 * stylesheet replaces the body of the object, which is its {@code φ}
 * attribute, with an atom, so that the {@code φ} of the object is the atom
 * itself. All the other attributes of the object stay as they were. When
 * the transpiler meets this atom, it names its class after the object, with
 * {@code φ} at the end, which is exactly the class name that
 * {@link Rendering} gave to the Java file.</p>
 *
 * <p>A changed file is written into the directory of patched sources,
 * under the same name as its source. A source where nothing changed is not
 * written at all. So a reader of the build finds in that directory only
 * what this module changed. The Maven goal then tells the transpiler to
 * read the patched copy instead of the source, but only where such a copy
 * exists. The copies are never deleted, so a copy from an earlier build
 * may still be in the directory, even when none of its objects can be an
 * atom any more. This is why this stage also writes the names of the
 * files it changed in this build into the file {@code patched.tsv}, next
 * to {@code rendered.tsv}. The Maven goal uses only the copies in that
 * list, and an old copy that is not in the list is ignored.</p>
 *
 * @since 0.74.0
 */
final class Patching implements Proc<Path> {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory where the patched XMIR files are written.
     */
    private final Path patched;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param dir The directory where the patched XMIR files are written
     */
    Patching(final Collection<Path> srcs, final Path dir) {
        this.sources = srcs;
        this.patched = dir;
    }

    @Override
    public void exec(final Path home) throws IOException {
        final Path rendered = home.resolve("rendered.tsv");
        if (!Files.exists(rendered)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while patching needs the entries the rendering wrote",
                    rendered
                )
            );
        }
        final XSL sheet = new XSLDocument(
            Patching.class.getResource("/org/eolang/lowering/patching.xsl"),
            "/org/eolang/lowering/patching.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("rendered", rendered.toUri().toString());
        final Collection<String> files = new ArrayList<>(0);
        int atoms = 0;
        for (final Path source : new Sorted<>(this.sources)) {
            final XML out = sheet.transform(new XMLDocument(source));
            final List<String> names = out.xpath(
                "//o[@name='φ'][o[@name='λ' and not(@atom)]]/../@name"
            );
            if (!names.isEmpty()) {
                final Path file = Files.createDirectories(this.patched)
                    .resolve(source.getFileName().toString());
                Files.write(file, out.toString().getBytes(StandardCharsets.UTF_8));
                files.add(String.format("%s%n", file.getFileName()));
                atoms += names.size();
                Logger.info(
                    this, "Patched %[file]s: %s",
                    file, String.join(", ", names)
                );
            }
        }
        Files.write(
            home.resolve("patched.tsv"),
            String.join("", files).getBytes(StandardCharsets.UTF_8)
        );
        Logger.info(
            this,
            "Put %d atom(s) into %d of %d XMIR files, patched into %[file]s",
            atoms, files.size(), this.sources.size(), this.patched
        );
    }
}
