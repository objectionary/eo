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
package org.eolang.maven.hash;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.URL;
import org.cactoos.Text;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Commit hashes hash-table as text from objectionary.
 */
public final class ChsAsText implements Text {
    /**
     * Cache.
     */
    private static String CACHE = "";
    /**
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    @Override
    public String asString() throws Exception {
        if (ChsAsText.CACHE.isEmpty()) {
            try {
                ChsAsText.CACHE = new UncheckedText(
                    new TextOf(
                        new URL(ChsAsText.HOME)
                    )
                ).asString();
            } catch (final IOException ex) {
                Logger.warn(
                    ChsAsText.class,
                    "Failed to load catalog of Git hashes from %s, because of %s: '%s'",
                    ChsAsText.HOME, ex.getClass().getSimpleName(), ex.getMessage()
                );
                ChsAsText.CACHE = "";
            }
        }
        return ChsAsText.CACHE;
    }
}
