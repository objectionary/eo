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
package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * {@link Universe} to be used from within native function.
 * Since we cannot save stacktrace of exception inside native code
 *  (we can only detect it), we should keep it on the java side.
 *  More detailed:
 *  <a href = "https://www.developer.com/open-source/exception-handling-in-jni/">
 *   www.developer.com/open-source/exception-handling-in-jni</a>,
 *  <a href = "https://stackoverflow.com/questions/2054598/how-to-catch-jni-java-exception/2125673#2125673">
 *   stackoverflow.com/questions/2054598/how-to-catch-jni-java-exception</a>,
 *  <a href = "https://www.iitk.ac.in/esc101/05Aug/tutorial/native1.1/implementing/error.html/">
 *   www.iitk.ac.in/esc101/05Aug/tutorial/native1.1/implementing/error.html</a>
 * @since 0.32
 * @checkstyle IllegalCatchCheck (200 lines)
 */
@Versionized
public final class UniverseSafe implements Universe {

    /**
     * {@link Universe} providing methods.
     */
    private final UniverseDefault origin;

    /**
     * Exception to be saved.
     */
    private final AtomicReference<Throwable> saved;

    /**
     * Ctor.
     * @param origin Origin.
     * @param throwable A cell to keep throwable.
     */
    public UniverseSafe(final UniverseDefault origin, final AtomicReference<Throwable> throwable) {
        this.origin = origin;
        this.saved = throwable;
    }

    @Override
    public int find(final String name) {
        try {
            return this.origin.find(name);
        } catch (final Throwable throwable) {
            this.saved.set(throwable);
            throw throwable;
        }
    }

    @Override
    public void put(final int vertex, final byte[] bytes) {
        try {
            this.origin.put(vertex, bytes);
        } catch (final Throwable throwable) {
            this.saved.set(throwable);
            throw throwable;
        }
    }

    @Override
    public void bind(final int parent, final int child, final String att) {
        try {
            this.origin.bind(parent, child, att);
        } catch (final Throwable throwable) {
            this.saved.set(throwable);
            throw throwable;
        }
    }

    @Override
    public int copy(final int vertex) {
        try {
            return this.origin.copy(vertex);
        } catch (final Throwable throwable) {
            this.saved.set(throwable);
            throw throwable;
        }
    }

    @Override
    public byte[] dataize(final int vertex) {
        try {
            return this.origin.dataize(vertex);
        } catch (final Throwable throwable) {
            this.saved.set(throwable);
            throw throwable;
        }
    }
}
