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
package org.eolang.maven.footprint;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Scalar;
import org.cactoos.scalar.IoChecked;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
import org.eolang.maven.Place;
import org.eolang.maven.util.Home;

/**
 * Program footprint of EO compilation process.
 * <p/>The footprint consists of file in {@link #main} folder and optionally cached
 * file in {@link #cache} folder.
 * Caching is applied if {@link #hash} is not empty otherwise caching is ignored.
 * <p/>Usage example:
 * <code>
 *  <pre>
 *    final Footprint footprint = new Footprint(
 *      hash,
 *      targetRoot,
 *      cacheRoot
 *    ).save(program, ext);
 *
 *    String content = footprint.content(program, ext);
 *  </pre>
 * </code>
 * @since 1.0
 */
public final class FtCached implements Footprint {
    /**
     * Path to target root.
     */
    private final Path main;

    /**
     * Version tag.
     */
    private final String hash;

    /**
     * Path to cache root.
     */
    private final Path cache;
    private final FtDefault origin;

    /**
     * Ctor.
     * @param hash Version tag
     * @param main Main root
     * @param cache Cache root
     */
    public FtCached(final String hash, final Path main, final Path cache) {
        this.hash = hash;
        this.main = main;
        this.cache = cache;
        this.origin = new FtDefault(this.main);
    }

    @Override
    public String load(final String program, final String ext) throws IOException {
        final Path cached = new Place(program).make(this.cache.resolve(this.hash), ext);
        final String content;
        if (cached.toFile().exists()) {
            content = new IoCheckedText(
                new TextOf(cached)
            ).asString();
        } else {
            content = origin.load(program, ext);
        }
        return content;
    }

    @Override
    public void save(final String program, final String ext, final Scalar<String> content)
        throws IOException {
        final Path cached = new Place(program).make(this.cache.resolve(this.hash), ext);
        final String text;
        if (cached.toFile().exists()) {
            Logger.debug(
                this,
                "Program %s.%s found in cache: %s",
                program, ext, cached
            );
            text = this.load(program, ext);
        } else {
            text = new IoChecked<>(content).value();
            new Home(this.cache).save(text, this.cache.relativize(cached));
        }
        origin.save(program, ext, () -> text);
    }
}
