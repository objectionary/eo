/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Place;

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
            final Path path = new Place(xml.xpath("/program/@name").get(0))
                .make(this.folder, AssembleMojo.ATTR_XMIR);
            final XML optimized;
            if (Files.exists(path)) {
                optimized = new XMLDocument(path);
            } else {
                optimized = this.delegate.apply(xml);
                Files.createDirectories(path.getParent());
                Files.createFile(path);
                Files.write(path, optimized.toString().getBytes(StandardCharsets.UTF_8));
            }
            return optimized;
        } catch (final IOException ex) {
            throw new IllegalStateException(String.format("Can't optimize '%s'", xml), ex);
        }
    }
}
