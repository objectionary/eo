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
 * @param <T> Data type.
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (200 lines)
 */
public interface Data<T> {

    /**
     * Take the data.
     * @return The data
     */
    T take();

    /**
     * Data being returned only once, from encapsulated object.
     *
     * @param <T> The type of data
     * @since 0.1
     */
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
        public int hashCode() {
            return this.take().hashCode();
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

    /**
     * Makes a {@code Phi} out of a Java object, like String or integer.
     *
     * This is more convenient than making EOstring and then
     * injecting "Δ" into it. This class is used in Java tests mostly
     * for the sake of brevity. In auto-generated Java code we use EOint/EOstring
     * and then inject "Δ" with Data.Value into it.
     *
     * @since 0.1
     */
    final class ToPhi implements Phi {

        private final Phi value;

        private final Phi object;

        public ToPhi(final Object obj) {
            this.value = new Data.Value<>(obj);
            this.object = Data.ToPhi.toPhi(obj);
            this.object.attr("Δ").put(this.value);
        }

        @Override
        public boolean equals(final Object obj) {
            return this.value.equals(obj);
        }

        @Override
        public int hashCode() {
            return this.value.hashCode();
        }

        @Override
        public Phi copy() {
            return this;
        }

        @Override
        public void move(final Phi rho) {
            // nothing
        }

        @Override
        public Attr attr(final int pos) {
            return this.object.attr(pos);
        }

        @Override
        public Attr attr(final String name) {
            return this.object.attr(name);
        }

        @Override
        public String φTerm() {
            return this.object.φTerm();
        }

        @Override
        public String toString() {
            return this.object.toString();
        }

        private static Phi toPhi(final Object obj) {
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
            } else if (obj instanceof Number) {
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
            return phi;
        }
    }

    /**
     * A single value as {@code Phi}.
     *
     * @param <T> The type of data
     * @since 0.1
     */
    final class Value<T> extends PhDefault implements Data<T> {

        private final T val;

        public Value(final T value) {
            super(Phi.Φ);
            this.val = value;
            this.vertex = PhDefault.VTX.best(value);
        }

        @Override
        public String φTerm() {
            final String txt;
            if (this.val instanceof Term) {
                txt = Term.class.cast(this.val).φTerm();
            } else if (this.val instanceof Phi[]) {
                final StringBuilder out = new StringBuilder(0);
                final Phi[] items = Phi[].class.cast(this.val);
                for (int idx = 0; idx < items.length; ++idx) {
                    if (out.length() > 0) {
                        out.append(",\n");
                    }
                    out.append('ι').append(idx).append(" ↦ ");
                    if (items[idx] == null) {
                        out.append('Ø');
                    } else {
                        out.append(items[idx].φTerm());
                    }
                }
                txt = String.format("⟦\n\t%s\n⟧", out.toString());
            } else {
                txt = this.toString()
                    .replace("⟦", "\\uE29FA6")
                    .replace("⟧", "\\uE29FA7")
                    .replace(", ", "\\u2C ");
            }
            return txt;
        }

        @Override
        public String toString() {
            final String txt;
            if (this.val instanceof String) {
                txt = String.format(
                    "\"%s\"",
                    this.val.toString()
                        .replace("\n", "\\n")
                        .replace("\r", "\\r")
                );
            } else if (this.val instanceof byte[]) {
                final StringBuilder out = new StringBuilder(0);
                for (final byte data : (byte[]) this.val) {
                    if (out.length() > 0) {
                        out.append('-');
                    }
                    out.append(String.format("%02X", data));
                }
                if (out.length() == 0) {
                    out.append('-');
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
