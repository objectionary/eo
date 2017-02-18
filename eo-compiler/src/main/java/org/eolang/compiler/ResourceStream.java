/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler;

import java.io.IOException;
import java.io.InputStream;

/**
 * Class comment.
 *
 * @author John Page (johnpagedev@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class ResourceStream extends InputStream {

    /**
     * Attribute comment.
     */
    private final InputStream origin;

    /**
     * Method comment.
     *
     * @param resource R.
     * @throws IOException If.
     */
    @SuppressWarnings("PMD.CallSuperInConstructor")
    public ResourceStream(final String resource) throws IOException {
        this(stream(resource));
    }

    /**
     * Method comment.
     *
     * @param origin O.
     */
    @SuppressWarnings("PMD.CallSuperInConstructor")
    private ResourceStream(final InputStream origin) {
        this.origin = origin;
    }

    @Override
    public int read() throws IOException {
        return this.origin.read();
    }

    @Override
    public int read(final byte[] bytes) throws IOException {
        return this.origin.read(bytes);
    }

    @Override
    public int read(final byte[] bytes, final int off, final int len)
        throws IOException {
        return this.origin.read(bytes, off, len);
    }

    @Override
    public void close() throws IOException {
        this.origin.close();
    }

    /**
     * Method comment.
     *
     * @param resource O.
     * @return Something.
     * @throws IOException If.
     */
    private static InputStream stream(final String resource)
        throws IOException {
        InputStream input = Thread.currentThread()
            .getContextClassLoader().getResourceAsStream(resource);
        if (input == null) {
            input = ResourceStream.class.getResourceAsStream(resource);
        }
        if (input == null) {
            throw new IOException();
        }
        return input;
    }
}
