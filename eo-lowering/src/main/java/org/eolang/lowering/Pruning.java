/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import org.cactoos.Proc;
import org.cactoos.iterable.Sorted;

/**
 * The stage that removes the tests from copies of all the sources.
 *
 * <p>In EO, a test is written inside the object it tests. But a test is
 * really a separate small program: it makes the objects it needs, runs
 * them, and checks the result. No other object uses what a test makes.
 * This module is interested only in the objects themselves. If the tests
 * stayed, phino would spend a lot of time on them, and it would learn
 * nothing useful from that work. In fact, this is what happened with
 * eo-runtime: phino got stuck on the tests of the very first objects.
 * So, the tests are removed before any other stage starts. The parser
 * marks every test with a special name, and this is how this stage finds
 * them.</p>
 *
 * <p>The original sources are never changed. For every source, this stage
 * writes a copy without the tests into the directory
 * {@code NN-lowering-planting}, under the same file name as the source. All
 * the next stages read these copies. If two sources have the same file
 * name, they would need the same copy, and one of them would be lost
 * without any warning. So, in that case, this stage fails the build. The
 * copies of an earlier build are deleted first. Without this, a source
 * that was deleted from the project would still be in the directory, and
 * the next stages would still read it.</p>
 *
 * @since 0.74.0
 */
final class Pruning implements Proc<Path> {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     */
    Pruning(final Collection<Path> srcs) {
        this.sources = srcs;
    }

    @Override
    public void exec(final Path target) throws IOException {
        final XSL sheet = new XSLDocument(
            Pruning.class.getResource("/org/eolang/lowering/pruning.xsl"),
            "/org/eolang/lowering/pruning.xsl"
        );
        final Collection<String> names = new HashSet<>(this.sources.size());
        final Path home = Files.createDirectories(target.resolve("7-lowering-planting"));
        for (final Path stale : new Copies(target)) {
            Files.delete(stale);
        }
        for (final Path source : new Sorted<>(this.sources)) {
            if (!names.add(source.getFileName().toString())) {
                throw new IllegalStateException(
                    String.format(
                        "The source '%s' is named like another one of the build, while the pruning keeps one copy per name",
                        source
                    )
                );
            }
            Files.write(
                home.resolve(source.getFileName().toString()),
                sheet.transform(new XMLDocument(source)).toString()
                    .getBytes(StandardCharsets.UTF_8)
            );
        }
        Logger.info(
            this,
            "Cut the tests out of %d XMIR files into %[file]s",
            this.sources.size(),
            home
        );
    }
}
