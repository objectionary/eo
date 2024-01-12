/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.util.Optional;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Place;
import org.eolang.maven.footprint.FtDefault;

/**
 * The cached optimization.
 * Returns already optimized XML if it's found in the cache.
 *
 * @since 0.28.11
 */
public final class OptCached implements Optimization {

    /**
     * Real optimization.
     */
    private final Optimization delegate;

    /**
     * Cache folder.
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
    public XML apply(final XML xml) {
        try {
            final XML optimized;
            if (this.contains(xml)) {
                optimized = new XMLDocument(this.cached(xml));
            } else {
                optimized = this.delegate.apply(xml);
                new FtDefault(this.folder).save(
                    xml.xpath("/program/@name").get(0),
                    AssembleMojo.IR_EXTENSION,
                    optimized::toString
                );
            }
            return optimized;
        } catch (final IOException ex) {
            throw new IllegalStateException(String.format("Can't optimize '%s'", xml), ex);
        }
    }

    /**
     * Returns the path to the cached program.
     * Pay attention that the path is not checked for existence.
     * @param xml Eo program.
     * @return Path to the cached program.
     */
    private Path cached(final XML xml) {
        return new Place(xml.xpath("/program/@name").get(0))
            .make(this.folder, AssembleMojo.IR_EXTENSION);
    }

    /**
     * Checks if the cache contains the program.
     * @param xml Eo program.
     * @return True if the cache contains the program.
     * @throws IOException If fails.
     */
    private boolean contains(final XML xml) throws IOException {
        final Path path = this.cached(xml);
        final Optional<String> hash = xml.xpath("/program/@hash").stream().findFirst();
        final boolean res;
        if (Files.exists(path) && hash.isPresent()) {
            res = new XMLDocument(path).xpath("/program/@hash").stream().findFirst()
                .equals(hash);
        } else {
            res = false;
        }
        return res;
    }
}
