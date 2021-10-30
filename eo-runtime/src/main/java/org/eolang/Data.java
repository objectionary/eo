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

package org.eolang;

import EOorg.EOeolang.EOarray;
import EOorg.EOeolang.EObool;
import EOorg.EOeolang.EObytes;
import EOorg.EOeolang.EOchar;
import EOorg.EOeolang.EOfloat;
import EOorg.EOeolang.EOint;
import EOorg.EOeolang.EOregex;
import EOorg.EOeolang.EOstring;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.regex.Pattern;

/**
 * A data container.
 *
 * @since 0.1
 */
public interface Data<T> {

    /**
     * Take the data.
     * @return The data
     */
    T take();

    final class Once<T> implements Data<T> {
        private final Data<T> src;
        private final AtomicReference<T> ref;
        private final Supplier<String> blank;
        public Once(final Data<T> data, final Supplier<String> txt) {
            this.src = data;
            this.ref = new AtomicReference<>();
            this.blank = txt;
        }
        @Override
        public String toString() {
            final T data = this.ref.get();
            String txt = this.blank.get();
            if (txt.isEmpty()) {
                txt = this.src.take().toString();
            } else if (data != null) {
                txt = data.toString();
            }
            return txt;
        }
        @Override
        public T take() {
            if (this.ref.get() == null) {
                this.ref.set(this.src.take());
            }
            return this.ref.get();
        }
    }

    final class ToPhi extends PhOnce {
        public ToPhi(final Object obj) {
            super(
                () -> {
                    final Phi phi;
                    if (obj instanceof Boolean) {
                        phi = new EObool(Phi.Φ);
                    } else if (obj instanceof byte[]) {
                        phi = new EObytes(Phi.Φ);
                    } else if (obj instanceof Long) {
                        phi = new EOint(Phi.Φ);
                    } else if (obj instanceof String) {
                        phi = new EOstring(Phi.Φ);
                    } else if (obj instanceof Character) {
                        phi = new EOchar(Phi.Φ);
                    } else if (obj instanceof Double) {
                        phi = new EOfloat(Phi.Φ);
                    } else if (obj instanceof Pattern) {
                        phi = new EOregex(Phi.Φ);
                    } else if (obj instanceof Phi[]) {
                        phi = new EOarray(Phi.Φ);
                    } else {
                        throw new IllegalArgumentException(
                            String.format(
                                "Unknown type of data: %s",
                                obj.getClass().getCanonicalName()
                            )
                        );
                    }
                    return new PhWith(phi, "Δ", new Data.Value<>(obj));
                },
                () -> ""
            );
        }
    }

    final class Value<T> extends PhDefault implements Data<T> {
        private final T val;
        public Value(final T value) {
            super(Phi.Φ);
            this.val = value;
        }
        @Override
        public String toString() {
            final String txt;
            if (this.val instanceof String) {
                txt = String.format("\"%s\"", this.val.toString());
            } else if (this.val instanceof byte[]) {
                final StringBuilder out = new StringBuilder(0);
                for (final byte data : (byte[]) this.val) {
                    if (out.length() > 0) {
                        out.append('-');
                    }
                    out.append(String.format("%02X", data));
                }
                txt = out.toString();
            } else if (this.val.getClass().isArray()) {
                txt = Arrays.toString((Object[]) this.val);
            } else {
                txt = this.val.toString();
            }
            return txt;
        }
        @Override
        public T take() {
            return this.val;
        }
    }

}
