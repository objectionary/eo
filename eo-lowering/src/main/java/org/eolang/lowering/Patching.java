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
import java.util.Collection;
import javax.xml.transform.stream.StreamSource;
import org.cactoos.Proc;
import org.cactoos.iterable.Sorted;

/**
 * The putting of an atom in the place of every body rendered into Java.
 *
 * <p>This is the last stage, since only a body the rendering wrote a class
 * for may be taken away: a formation whose entry is a taint, or whose run
 * was killed, keeps its body, and the transpiler compiles it as it always
 * did. The patch is made of the XMIR files of the build, with their tests,
 * and of the list of the entries the rendering wrote, and phino's morphed
 * program is never read. What {@code patching.xsl} does to one file is
 * all there is to the patch: a formation of a rendered entry gets the atom
 * {@code l🌵N}, named after the number of its entry, and its {@code φ}
 * becomes {@code ξ.l🌵N}, so the transpiler, meeting an atom, refers to the
 * very class the rendering named after it.</p>
 *
 * <p>A patched file is written into the directory of patched sources, under
 * the name of its source, and a source with nothing patched in it is not
 * written at all, so a reader of the build finds there only what the
 * lowering changed, and the goal of the plugin, which names that directory,
 * points the transpiler at a patched copy only where there is one. The
 * copies of an earlier build are deleted first, so that a source whose
 * formations are all taints now is read from where it was.</p>
 *
 * @since 0.74.0
 */
final class Patching implements Proc<Path> {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory the patched XMIR files are written into.
     */
    private final Path patched;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param dir The directory the patched XMIR files are written into
     */
    Patching(final Collection<Path> srcs, final Path dir) {
        this.sources = srcs;
        this.patched = dir;
    }

    @Override
    public void exec(final Path target) throws IOException {
        final Path rendered = target.resolve("7-lowering").resolve("rendered.tsv");
        if (!Files.exists(rendered)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while patching needs the entries the rendering wrote",
                    rendered
                )
            );
        }
        new Wiping().exec(this.patched);
        final XSL sheet = new XSLDocument(
            Patching.class.getResource("/org/eolang/lowering/patching.xsl"),
            "/org/eolang/lowering/patching.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("rendered", rendered.toUri().toString());
        int files = 0;
        int atoms = 0;
        for (final Path source : new Sorted<>(this.sources)) {
            final XML out = sheet.transform(new XMLDocument(source));
            final int found = out.nodes("//o[starts-with(@name, 'l🌵')][o[@name='λ']]").size();
            if (found > 0) {
                final Path file = Files.createDirectories(this.patched)
                    .resolve(source.getFileName().toString());
                Files.write(file, out.toString().getBytes(StandardCharsets.UTF_8));
                files += 1;
                atoms += found;
                Logger.info(
                    this, "Put %d atom(s) into %[file]s, patched into %[file]s",
                    found, source, file
                );
            }
        }
        Logger.info(
            this,
            "Put %d atom(s) into %d of %d XMIR files, patched into %[file]s",
            atoms, files, this.sources.size(), this.patched
        );
    }
}
