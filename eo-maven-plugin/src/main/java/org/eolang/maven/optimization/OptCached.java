/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Place;
import org.eolang.maven.footprint.FtDefault;

/**
 * The cached optimization.
 * Returns already optimized XML if it's found in the cache.
 *
 * @since 0.28.11
 * @todo #2746:30min Unify caching mechanism on stages: parse, optimize, pull and so on.
 *  Current implementations of caching on parsing stage and optimize stages work differently.
 *  In ParseMojo we have condition {@code if (tojo.hasHash()) }, in OptimizeMojo or ShakeMojo we
 *  compare creation time of files.
 *  Don't forget to enable the tests.
 * @todo #2790:30min Get the XML name from its path,
 * but doesn't  use {@code xml.xpath("/program/@name").get(0)},
 * in classes {@link OptCached}, {@link OptTrain}, {@link OptSpy}.
 */
public final class OptCached implements Optimization {

    /**
     * Real optimization.
     */
    private final Optimization delegate;

    /**
     * Absolute path of cache folder.
     */
    private final Path folder;

    /**
     * The main constructor.
     *
     * @param delegate Real optimization.
     * @param folder Cache folder.
     */
    public OptCached(
        final Optimization delegate,
        final Path folder
    ) {
        this.delegate = delegate;
        this.folder = folder;
    }

    @Override
    public XML apply(final Path path) throws Exception {
        try {
            final XML optimized;
            if (this.contains(path)) {
                optimized = new XMLDocument(this.cached(path));
            } else {
                optimized = this.delegate.apply(path);
                new FtDefault(this.folder).save(
                    new XMLDocument(path).xpath("/program/@name").get(0),
                    AssembleMojo.IR_EXTENSION,
                    optimized::toString
                );
            }
            return optimized;
        } catch (final IOException ex) {
            throw new IOException(
                String.format("Can't optimize '%s'", path),
                ex
            );
        }
    }

    /**
     * Returns the path to the cached program.
     * Pay attention that the path is not checked for existence.
     * @param path Path eo program.
     * @return Path to the cached program.
     * @throws IOException If fails.
     */
    private Path cached(final Path path) throws IOException {
        return new Place(
            new XMLDocument(path)
                .xpath("/program/@name").get(0)
        )
            .make(this.folder, AssembleMojo.IR_EXTENSION);
    }

    /**
     * Checks if the cache contains the program.
     * @param path Path eo program.
     * @return True if the cache contains the program.
     * @throws IOException If fails.
     */
    private boolean contains(final Path path) throws IOException {
        final Path cache = this.cached(path);
        final boolean res;
        if (Files.exists(cache)) {
            res = !Files
                .getLastModifiedTime(cache)
                .toInstant()
                .isBefore(
                    Files.getLastModifiedTime(path)
                        .toInstant()
                    );
        } else {
            res = false;
        }
        return res;
    }
}
