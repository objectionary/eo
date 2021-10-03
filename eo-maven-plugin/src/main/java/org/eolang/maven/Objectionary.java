/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.net.URL;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * The abstraction of the Objectionary server.
 *
 * @since 0.1
 */
public final class Objectionary implements Func<String, Input> {

    @Override
    public Input apply(final String name) throws Exception {
        final URL url = new URL(
            String.format(
                // @checkstyle LineLength (1 line)
                "https://raw.githubusercontent.com/yegor256/objectionary/master/objects/%s.eo",
                name.replace(".", "/")
            )
        );
        Logger.info(
            this, "The object '%s' will be pulled from %s",
            name, url
        );
        return new InputOf(url);
    }

}
