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
import java.util.function.Supplier;

/**
 * An object wrapping another one.
 *
 * @since 0.1
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
class PhOnce implements Phi {

    /**
     * The object fetched.
     */
    private final Supplier<Phi> object;

    /**
     * As string representation.
     */
    private final Supplier<String> string;

    /**
     * As φ term representation.
     */
    private final Supplier<String> term;

    /**
     * Reference.
     */
    private final AtomicReference<Phi> ref;

    /**
     * Ctor.
     *
     * @param obj The object
     * @param str The "as string" value
     * @param trm The "as φ term" value
     */
    PhOnce(final Supplier<Phi> obj, final Supplier<String> str, final Supplier<String> trm) {
        this.ref = new AtomicReference<>(null);
        this.object = () -> {
            synchronized (this.ref) {
                if (this.ref.get() == null) {
                    this.ref.set(obj.get());
                }
                return this.ref.get();
            }
        };
        this.string = str;
        this.term = trm;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.object.get().equals(obj);
    }

    @Override
    public int hashCode() {
        return this.object.get().hashCode();
    }

    @Override
    public final String toString() {
        return this.string.get();
    }

    @Override
    public final String φTerm() {
        return this.term.get();
    }

    @Override
    public Phi copy() {
        return new PhOnce(
            () -> this.object.get().copy(),
            () -> String.format("%s'", this.string),
            this.term
        );
    }

    @Override
    public Phi take(final String name) {
        return this.object.get().take(name);
    }

    @Override
    public boolean put(final int pos, final Phi obj) {
        return this.object.get().put(pos, obj);
    }

    @Override
    public boolean put(final String name, final Phi obj) {
        return this.object.get().put(name, obj);
    }

    @Override
    public String locator() {
        return this.object.get().locator();
    }

    @Override
    public String forma() {
        return this.object.get().forma();
    }

    @Override
    public byte[] delta() {
        return this.object.get().delta();
    }
}
