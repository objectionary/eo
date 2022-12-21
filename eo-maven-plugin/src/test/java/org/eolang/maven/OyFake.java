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
import org.cactoos.Func;
import org.cactoos.Input;
import org.eolang.maven.objectionary.Objectionary;

/**
 * Objectionary with lambda-function Ctor-s for testing.
 *
 * @since 0.28.11
 * @checkstyle IllegalCatchCheck (150 lines)
 */
public final class OyFake implements Objectionary {

    /**
     * Function that emulates 'get()' method in {@link Objectionary}.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Func<String, Input> get;

    /**
     * Function that emulates 'contains()' method in {@link Objectionary}.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Func<String, Boolean> contains;

    /**
     * Ctor.
     *
     * @param get Lambda func for get()
     */
    public OyFake(final Func<String, Input> get) {
        this(
            get,
            s -> true
        );
    }

    /**
     * Ctor.
     *
     * @param get Lambda func for get()
     * @param cont Lambda func for contains()
     */
    public OyFake(final Func<String, Input> get, final Func<String, Boolean> cont) {
        this.get = get;
        this.contains = cont;
    }

    @Override
    public String toString() {
        return "OyFake";
    }

    @Override
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public Input get(final String name) {
        try {
            return this.get.apply(name);
        } catch (final Exception ex) {
            Logger.debug(
                this, "Invalid lambda function for get() method in OyFake!"
            );
            throw new IllegalArgumentException(ex);
        }
    }

    @Override
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public boolean contains(final String name) {
        try {
            return this.contains.apply(name);
        } catch (final Exception ex) {
            Logger.debug(
                this, "Invalid lambda function for contains() method in OyFake!"
            );
            throw new IllegalArgumentException(ex);
        }
    }

}
