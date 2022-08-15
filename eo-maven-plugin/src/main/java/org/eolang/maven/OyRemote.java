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
import java.net.MalformedURLException;
import java.net.URL;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * The simple HTTP Objectionary server.
 *
 * @since 0.1
 */
public final class OyRemote implements Objectionary {

    /**
     * The address template.
     */
    private final String template;

    /**
     * Constructor.
     * @param tag Tag
     * @throws IOException if fails.
     */
    public OyRemote(final String tag) throws IOException {
        this.template = String.format(
            "https://raw.githubusercontent.com/objectionary/home/%s/objects/%%s.eo",
            tag
        );
    }

    @Override
    public String toString() {
        return this.template;
    }

    @Override
    public Input get(final String name) throws MalformedURLException {
        final URL url = new URL(
            String.format(this.template, name.replace(".", "/"))
        );
        Logger.debug(
            this, "The object '%s' will be pulled from %s...",
            name, url
        );
        return new InputOf(url);
    }

}
