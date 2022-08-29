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
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;

/**
 * Program footprint in EO compilation process.
 * The footprint consists of file in the {@link #main} folder and optionally cached
 * file in {@link #cache} folder.
 * Caching is applied if {@link #ver} represents true version in terms of
 * {@link #versioned(String)} method.
 * <br/>Usage example:
 * <code>
 *  <pre>
 *    final Footprint footprint = new Footprint(
 *      version,
 *      targetRoot,
 *      cacheRoot
 *    ).save(program, ext);
 *  </pre>
 * </code>
 * @since 1.0
 */
public class Footprint {
    /**
     * Path to target root.
     */
    private final Path main;

    /**
     * Version tag.
     */
    private final String ver;

    /**
     * Path to cache root.
     */
    private final Path cache;

    /**
     * Ctor.
     * @param ver Version tag
     * @param main Main root
     * @param cache Cache root
     */
    public Footprint(final String ver, final Path main, final Path cache) {
        this.ver = ver;
        this.main = main;
        this.cache = cache;
    }

    /**
     * Get content of the program.
     * @param program Program name
     * @param ext File extension
     * @return Content of a file
     * @throws IOException In case of IO issue.
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public String content(final String program, final String ext) throws IOException {
        final Path cached = new Place(program).make(this.cache.resolve(this.safeVer()), ext);
        final Path target = new Place(program).make(this.main, ext);
        final IoCheckedText content;
        if (Footprint.versioned(this.ver) && cached.toFile().exists()) {
            content = new IoCheckedText(
                new TextOf(cached)
            );
        } else {
            content = new IoCheckedText(
                new TextOf(target)
            );
        }
        return content.asString();
    }

    /**
     * Save content.
     * @param program Program name
     * @param ext File extension
     * @param content File content
     * @throws IOException In case of IO issues
     */
    public void save(final String program, final String ext, final Supplier<String> content)
        throws IOException {
        final Path cached = new Place(program).make(this.cache.resolve(this.safeVer()), ext);
        final Path target = new Place(program).make(this.main, ext);
        final String text;
        if (Footprint.versioned(this.ver) && cached.toFile().exists()) {
            Logger.info(
                this,
                "File found in cache: %s",
                cached
            );
            text = this.content(program, ext);
        } else {
            text = content.get();
            if (Footprint.versioned(this.ver)) {
                new Save(
                    text,
                    cached
                ).save();
            }
        }
        new Save(
            text,
            target
        ).save();
    }

    /**
     * Transform version for legal path.
     * @return Version tag
     */
    private String safeVer() {
        return this.ver.replaceAll("\\*", "_");
    }

    /**
     * Is it a true version?
     * @param ver Version to check
     * @return True if version has meaningful value and false otherwise
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    private static boolean versioned(final String ver) {
        final String trimmed = ver.trim();
        return !trimmed.isEmpty()
            && !trimmed.equals("0.0.0")
            && !trimmed.equals("*.*.*");
    }
}
