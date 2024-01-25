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
 *  In ParseMojo we have condition {@code if (tojo.hasHash())}, in OptimizeMojo or ShakeMojo we
 *  compare creation time of files.
 *  Don't forget to enable the tests.
 * @todo #2790:30min Refactor OptCache Class
 *  Currently, we have some concerns about the implementation of OptCache.
 *  It appears that the code is a bit too complicated.
 *  The OptCache class takes XMIR as an argument in OptCache#apply
 *  and the path to the same XMIR in the constructor, which seems odd.
 *  We need to consider how to refactor this class.
 *  Furthermore, the current implementation of OptCache
 *  and OptimizationTask has a similar logic of returning either from the cache
 *  or applying a default optimization.
 *  For more information, please refer to this discussion:
 *  <a href=“https://github.com/objectionary/eo/pull/2808#discussion_r1464941944”>issue</a>.
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
     * Absolute path of program xml.
     */
    private final Path path;

    /**
     * The main constructor.
     *
     * @param delegate Real optimization.
     * @param folder Cache folder.
     * @param path XML file path.
     */
    public OptCached(
        final Optimization delegate,
        final Path folder,
        final Path path
    ) {
        this.delegate = delegate;
        this.folder = folder;
        this.path = path;
    }

    @Override
    public XML apply(final XML xml) {
        final String name = xml.xpath("/program/@name").get(0);
        try {
            final XML optimized;
            if (this.contains(name)) {
                optimized = new XMLDocument(this.cached(name));
            } else {
                optimized = this.delegate.apply(xml);
                new FtDefault(this.folder).save(
                    name,
                    AssembleMojo.IR_EXTENSION,
                    optimized::toString
                );
            }
            return optimized;
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't optimize '%s'", xml),
                ex
            );
        }
    }

    /**
     * Returns the path to the cached program.
     * Pay attention that the path is not checked for existence.
     * @param name Name XML program.
     * @return Path to the cached program.
     * @throws IOException If fails.
     */
    private Path cached(final String name) throws IOException {
        return new Place(name)
            .make(this.folder, AssembleMojo.IR_EXTENSION);
    }

    /**
     * Checks if the cache contains the program.
     * @param name Name XML program.
     * @return True if the cache contains the program.
     * @throws IOException If fails.
     */
    private boolean contains(final String name) throws IOException {
        final Path cache = this.cached(name);
        final boolean res;
        if (Files.exists(cache)) {
            res = !Files
                .getLastModifiedTime(cache)
                .toInstant()
                .isBefore(
                    Files.getLastModifiedTime(this.path)
                        .toInstant()
                    );
        } else {
            res = false;
        }
        return res;
    }
}
