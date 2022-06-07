/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import java.security.InvalidParameterException;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * The simple HTTP Objectionary server.
 *
 * @since 0.1
 */
final 
public final class OyRemote implements Objectionary {

    /**
     * The hash code to fetch.
     */
    private final String hash;

    /**
     * The address template.
     */
    private final String addr;

    /**
     * Ctor.
     */
    public OyRemote() {
        this("master");
    }

    /*
     * Ctor.
     * @Param hsh the hash or branch, throws an error if hash is abbreviated
     * @checkstyle MagicNumber (10 lines)
     * @checkstyle ConstructorOnlyInitializesOrCallOtherConstructors (10 lines)
     * @checkstyle OnlyOneConstructorShouldDoInitialization (10 lines)
     */
    public OyRemote(final String hsh) throws InvalidParameterException {
        if (hsh.length() != 40) {  // 40 is length of correct hash
            throw new InvalidParameterException();
        }
        this.addr = "https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo";
        this.hash = hsh;
    }

    /**
     * Ctor.
     * @Param url The url template.
     * @Param hsh The hash or branch.
     */
    public OyRemote(final String url, final String hsh) {
        this.addr = url;
        this.hash = hsh;
    }

    @Override
    public Input get(final String name) throws IOException {
        final URL url = new URL(
            String.format(
                this.addr,
                this.hash, name.replace(".", "/")
            )
        );
        Logger.debug(
            this, "The object '%s' will be pulled from %s...",
            name, url
        );
        return new InputOf(url);
    }

}
