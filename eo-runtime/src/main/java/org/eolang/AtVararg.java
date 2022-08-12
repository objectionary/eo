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

package org.eolang;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Vararg attribute.
 *
 * @since 0.1
 */
public final class AtVararg implements Attr {

    /**
     * Array of items provided.
     */
    private final List<Phi> array;

    /**
     * Ctor.
     */
    public AtVararg() {
        this(new LinkedList<>());
    }

    /**
     * Ctor.
     * @param list List of them
     */
    private AtVararg(final List<Phi> list) {
        this.array = list;
    }

    @Override
    public String toString() {
        return String.format("%sV", this.array.toString());
    }

    @Override
    public String φTerm() {
        return new Data.Value<>(
            this.array.toArray(new Phi[this.array.size()])
        ).φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtVararg();
    }

    @Override
    public Phi get() {
        return new Data.ToPhi(this.array.toArray(new Phi[this.array.size()]));
    }

    @Override
    public void put(final Phi phi) {
        if (phi instanceof PhUnvar) {
            this.array.clear();
            this.array.addAll(
                Arrays.asList(
                    new Dataized(phi).take(Phi[].class)
                )
            );
        } else {
            this.array.add(phi);
        }
    }

}
