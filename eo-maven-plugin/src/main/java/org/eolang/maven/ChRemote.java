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
import java.net.URL;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Hash of tag from objectionary.
 *
 * @since 0.26
 * @todo #1382:90min We definitely have to split {@link org.eolang.maven.ChRemote} class to
 *   a two different classes: ChRemote and ChCached - in that case we will able to remove
 *   static methods and reuse ChCached somewhere else (not only in ChRemote).
 *   After implementation of ChCached - it's important remove CACHE from ChRemote.
 */
final class ChRemote implements CommitHash {

    /**
     * Cached text of hashes.
     */
    private static final String CACHE = ChRemote.safeLoad();

    /**
     * The URL where the list is kept.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Tag.
     */
    private final String tag;

    /**
     * Constructor.
     *
     * @param tag Tag
     */
    ChRemote(final String tag) {
        this.tag = tag;
    }

    @Override
    public String value() {
        final String result = new ChText(() -> ChRemote.CACHE, this.tag).value();
        if (result == null) {
            throw new IllegalArgumentException(
                String.format(
                    "Tag '%s' doesn't exist or the list of all tags was not loaded correctly",
                    this.tag
                )
            );
        }
        Logger.debug(this, "Git sha of %s is %s", this.tag, result);
        return result;
    }

    /**
     * Load all hashes and tags.
     *
     * @return Map of them (hash -> tag)
     */
    private static String safeLoad() {
        String cache;
        try {
            cache = new UncheckedText(ChRemote.load()).asString();
        } catch (final IOException ex) {
            Logger.warn(
                ChRemote.class,
                "Failed to load catalog of Git hashes from %s, because of %s: '%s'",
                ChRemote.HOME, ex.getClass().getSimpleName(), ex.getMessage()
            );
            cache = "";
        }
        return cache;
    }

    /**
     * Load all hashes and tags.
     *
     * @return Map of them (hash -> tag)
     * @throws IOException if fails
     */
    private static Text load() throws IOException {
        return new TextOf(new InputOf(new URL(ChRemote.HOME).openStream()));
    }
}
