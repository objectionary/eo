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
package org.eolang.maven.hash;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.cactoos.text.Sticky;
import org.cactoos.text.TextEnvelope;
import org.cactoos.text.TextOf;

/**
 * Commit hashes table as text from objectionary.
 *
 * <p>This class serves the purpose of the global cache in order to avoid
 * downloading the list of tags multiple times from objectionary.</p>
 *
 * @since 0.29.6
 */
final class CommitHashesText extends TextEnvelope {

    /**
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Constructor.
     */
    CommitHashesText() {
        super(new Sticky(CommitHashesText::asText));
    }

    /**
     * Download from the URL and return the content.
     *
     * @return The body of the web page
     */
    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static String asText() throws Exception {
        String hashes;
        try {
            hashes = new TextOf(new URL(CommitHashesText.HOME)).asString();
        } catch (final java.net.UnknownHostException ex) {
            Logger.warn(
                CommitHashesText.class,
                "Can't load hashes: %[exception]s",
                ex
            );
            hashes = CommitHashesMap.FAKES;
        }
        return hashes;
    }
}
