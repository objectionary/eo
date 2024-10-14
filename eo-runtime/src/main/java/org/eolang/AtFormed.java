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
 * Attribute that constructs object lazily.
 *
 * <p>The main difference between this attribute and {@link AtComposite} is
 * it does not depend on context. It means that every new copy of the attribute
 * is linked to the origin one (which was initialized first).</p>
 *
 * @since 0.36.0
 */
public final class AtFormed implements Attr {
    /**
     * Object.
     */
    private final AtomicReference<Phi> object;

    /**
     * Callback to retrieve object.
     */
    private final Supplier<Phi> callback;

    /**
     * Ctor.
     * @param func Callback to retrieve object.
     */
    public AtFormed(final Supplier<Phi> func) {
        this.object = new AtomicReference<>(null);
        this.callback = new SafeFunc<>(
            () -> this.object.updateAndGet(prev -> func.get())
        );
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtFormed(() -> this.get().copy());
    }

    @Override
    public Phi get() {
        if (this.object.get() == null) {
            this.callback.get();
        }
        return this.object.get();
    }

    @Override
    public boolean put(final Phi phi) {
        throw new ExReadOnly(
            "Formed attribute is read only"
        );
    }

    @Override
    public String φTerm() {
        return this.get().φTerm();
    }

    @Override
    public String toString() {
        return String.format("%sF", this.get().toString());
    }
}
