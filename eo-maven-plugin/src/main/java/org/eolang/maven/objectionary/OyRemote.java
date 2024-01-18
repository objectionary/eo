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
package org.eolang.maven.objectionary;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.io.InputWithFallback;
import org.eolang.maven.Place;
import org.eolang.maven.hash.CommitHash;

/**
 * The simple HTTP Objectionary server.
 *
 * @since 0.1
 */
public final class OyRemote implements Objectionary {

    /**
     * The address template.
     */
    private final UrlOy template;

    /**
     * Constructor.
     * @param hash Commit hash
     */
    public OyRemote(final CommitHash hash) {
        this.template = new UrlOy(
            "https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
            hash.value()
        );
    }

    @Override
    public String toString() {
        return this.template.toString();
    }

    @Override
    public Input get(final String name) throws MalformedURLException {
        final URL url = this.template.value(name);
        Logger.debug(
            this, "The object '%s' will be pulled from %s...",
            name, url
        );
        return new InputWithFallback(
            new InputOf(url),
            input -> {
                throw new IOException(
                    String.format(
                        "EO object '%s' is not found in this GitHub repository: https://github.com/objectionary/home. This means that you either misspelled the name of it or simply referred to your own local object somewhere in your code as if it was an object of 'org.eolang' package. Check the sources and make sure you always use +alias meta when you refer to an object outside of 'org.eolang', even if this object belongs to your package.",
                        name
                    ),
                    input
                );
            }
        );
    }

    @Override
    public boolean contains(final String name) throws IOException {
        final int code = ((HttpURLConnection) this.template.value(name).openConnection())
            .getResponseCode();
        return code >= HttpURLConnection.HTTP_OK && code < HttpURLConnection.HTTP_BAD_REQUEST;
    }

    /**
     * Objectionary URL template.
     *
     * <p>Assumes two placeholders in terms of
     * {@link String#format(String, Object...)}: 1st for version hash,
     * 2nd for program name, for
     * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">example</a>.</p>
     *
     * @since 1.0
     */
    public static final class UrlOy {

        /**
         * URL template.
         *
         * <p>Expects two placeholders in terms of
         * {@link String#format(String, Object...)}: 1st for hash,
         * 2nd for program name, for
         * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">example</a>.</p>
         */
        private final String template;

        /**
         * Objects version hash.
         */
        private final String hash;

        /**
         * Ctor.
         * @param template URL template.
         * @param hash Objects version hash.
         */
        public UrlOy(final String template, final String hash) {
            this.template = template;
            this.hash = hash;
        }

        /**
         * URL for the program.
         * @param name Fully qualified EO program name as specified by {@link Place}
         * @return URL
         * @throws MalformedURLException in case of incorrect URL
         */
        public URL value(final String name) throws MalformedURLException {
            return new URL(
                String.format(
                    this.template,
                    this.hash,
                    name.replace(".", "/")
                )
            );
        }

        @Override
        public String toString() {
            return this.template;
        }
    }

}
