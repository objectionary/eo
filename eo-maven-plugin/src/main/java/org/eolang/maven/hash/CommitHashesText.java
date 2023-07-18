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
import org.cactoos.text.Sticky;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Commit hashes table as text from objectionary.
 *
 * @since 0.29.6
 * @todo #1602:30min Come up with a good name for the class. There are many
 *  things we want to say with the name of the class: 1) it's commit hashes
 *  2) it's a text 3) it's loaded from the objectionary. The result name will be
 *  ObjectionaryCommitHashesText and it may look a bit verbose. Maybe it really
 *  does not but we should decide anyway.
 */
public final class CommitHashesText implements Text {
    /**
     * Cache.
     */
    private static final Text CACHE = new Sticky(CommitHashesText.load());

    /**
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    @Override
    public String asString() throws Exception {
        return CommitHashesText.CACHE.asString();
    }

    /**
     * Load commit hashes from objectionary only once.
     * @return Commit hashes from objectionary.
     */
    private static Text load() {
        return () -> {
            String text;
            try {
                text = new UncheckedText(
                    new TextOf(
                        new URL(CommitHashesText.HOME)
                    )
                ).asString();
            } catch (final IOException ex) {
                Logger.warn(
                    CommitHashesText.class,
                    "Failed to load catalog of Git hashes from %s, because of %s: '%s'",
                    CommitHashesText.HOME, ex.getClass().getSimpleName(), ex.getMessage()
                );
                text = "";
            }
            return text;
        };
    }
}
