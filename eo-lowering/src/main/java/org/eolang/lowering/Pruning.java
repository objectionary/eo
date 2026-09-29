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
 * The cutting of the tests out of every source of the build.
 *
 * <p>A test is a program of its own: it builds the objects it needs, runs
 * them and says what it expects, and nothing outside it copies what it
 * builds. Lowering folds what an object says about its inputs, and every
 * copy of that object standing in a test is one more copy the calculus
 * walks without a single formation of the build folding through it, which
 * is why the eo-runtime world stalled on the tests of its first objects.
 * So the tests, the bindings the parser marks as such, are cut out of
 * every source before anything is planted or merged, and the world holds
 * the objects and nothing that is said about them.</p>
 *
 * <p>The sources are never touched: a copy of each of them, with the tests
 * cut out, is written into {@code 7-lowering-planting} under the name of
 * the source, and it is those copies the stages after this one read. Two
 * sources named alike would share one copy and one of them would quietly
 * drop out of the world, so such a build fails here. The copies of an
 * earlier build are deleted first, since the later stages read whatever
 * the directory holds, and a source removed since then would otherwise
 * stay in the world.</p>
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
