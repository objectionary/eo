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

import EOorg.EOeolang.EObool;
import EOorg.EOeolang.EObytes;
import EOorg.EOeolang.EOfloat;
import EOorg.EOeolang.EOint;
import EOorg.EOeolang.EOstring;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * A data container.
 *
 * @param <T> Data type.
 * @since 0.1
 */
@Versionized
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

        /**
         * Data.
         */
        private final Data<T> src;

        /**
         * Reference.
         */
        private final AtomicReference<T> ref;

        /**
         * Blank supplier.
         */
        private final Supplier<String> blank;

        /**
         * Ctor.
         * @param data Data to return
         * @param txt Missing data text
         */
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
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            return this.take().equals(((Once<?>) obj).take());
        }

        @Override
        public String toString() {
            final T data = this.ref.get();
            String txt = this.blank.get();
            if (txt.isEmpty()) {
                txt = this.take().toString();
            } else if (data != null) {
                txt = data.toString();
            }
            return txt;
        }

        @Override
        public T take() {
            synchronized (this.ref) {
                return this.ref.updateAndGet(
                    t -> {
                        final T result;
                        if (t == null) {
                            result = Once.this.src.take();
                        } else {
                            result = t;
                        }
                        return result;
                    }
                );
            }
        }
    }

    /**
     * Makes a {@link Phi} out of a Java object, like {@link String} or {@link Integer}.
     *
     * <p>This is more convenient than making {@link EOstring} and then
     * injecting "Δ" into it. This class is used in Java tests mostly
     * for the sake of brevity. In auto-generated Java code we use
     * {@link EOint}/{@link EOstring}
     * and then inject "Δ" with {@link Data.Value} into it.
     *
     * @since 0.1
     */
    final class ToPhi implements Phi {

        /**
         * Data.
         */
        private final Phi value;

        /**
         * Phi object.
         */
        private final Phi object;

        /**
         * Ctor.
         * @param obj Data
         */
        public ToPhi(final Object obj) {
            this.value = new Data.Value<>(obj);
            this.object = Data.ToPhi.toPhi(obj, this.value);
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
        public Attr attr(final int pos) {
            return this.object.attr(pos);
        }

        @Override
        public Attr attr(final String name) {
            return this.object.attr(name);
        }

        @Override
        public String locator() {
            return this.object.locator();
        }

        @Override
        public String forma() {
            return this.object.forma();
        }

        @Override
        public String φTerm() {
            return this.object.φTerm();
        }

        @Override
        public String toString() {
            return this.object.toString();
        }

        /**
         * Convert to Phi object.
         * @param obj Object to convert
         * @param value Data value
         * @return Constructed Phi
         */
        private static Phi toPhi(final Object obj, final Phi value) {
            final Phi phi;
            byte[] bytes = new byte[0];
            final boolean delta;
            if (obj instanceof Boolean) {
                phi = new EObool(Phi.Φ);
                delta = false;
                if (obj.equals(true)) {
                    bytes = new byte[] {0x01};
                } else {
                    bytes = new byte[] {0x00};
                }
            } else if (obj instanceof byte[]) {
                phi = new EObytes(Phi.Φ);
                delta = true;
            } else if (obj instanceof Long) {
                phi = new EOint(Phi.Φ);
                delta = false;
                bytes = new BytesOf((Long) obj).take();
            } else if (obj instanceof String) {
                phi = new EOstring(Phi.Φ);
                delta = false;
                bytes = Data.ToPhi.unescapeJavaString(
                    (String) obj
                ).getBytes(StandardCharsets.UTF_8);
            } else if (obj instanceof Double) {
                phi = new EOfloat(Phi.Φ);
                delta = false;
                bytes = new BytesOf((Double) obj).take();
            } else {
                throw new IllegalArgumentException(
                    String.format(
                        "Unknown type of data: %s",
                        obj.getClass().getCanonicalName()
                    )
                );
            }
            if (delta) {
                phi.attr(Attr.DELTA).put(value);
            } else {
                final Phi bts = new EObytes(Phi.Φ);
                bts.attr(Attr.DELTA).put(new Data.Value<>(bytes));
                phi.attr(0).put(bts);
            }
            return phi;
        }

        /**
         * Unescapes a string that contains standard Java escape sequences.
         * <ul>
         * <li><strong>&#92;b &#92;f &#92;n &#92;r &#92;t &#92;" &#92;'</strong> :
         * BS, FF, NL, CR, TAB, double and single quote.</li>
         * <li><strong>&#92;X &#92;XX &#92;XXX</strong> : Octal character
         * specification (0 - 377, 0x00 - 0xFF).</li>
         * <li><strong>&#92;uXXXX</strong> : Hexadecimal based Unicode character.</li>
         * </ul>
         * @param str A string optionally containing standard java escape sequences.
         * @return The translated string
         * @checkstyle CyclomaticComplexityCheck (100 lines)
         * @checkstyle JavaNCSSCheck (100 lines)
         * @checkstyle NestedIfDepthCheck (100 lines)
         * @checkstyle ModifiedControlVariableCheck (100 lines)
         */
        private static String unescapeJavaString(final String str) {
            final StringBuilder unescaped = new StringBuilder(str.length());
            for (int idx = 0; idx < str.length(); ++idx) {
                char chr = str.charAt(idx);
                if (chr == '\\') {
                    final char next;
                    if (idx == str.length() - 1) {
                        next = '\\';
                    } else {
                        next = str.charAt(idx + 1);
                    }
                    if (next >= '0' && next <= '7') {
                        String code = String.valueOf(next);
                        ++idx;
                        if ((idx < str.length() - 1) && str.charAt(idx + 1) >= '0'
                            && str.charAt(idx + 1) <= '7') {
                            code += str.charAt(idx + 1);
                            ++idx;
                            if ((idx < str.length() - 1) && str.charAt(idx + 1) >= '0'
                                && str.charAt(idx + 1) <= '7') {
                                code += str.charAt(idx + 1);
                                ++idx;
                            }
                        }
                        unescaped.append((char) Integer.parseInt(code, 8));
                        continue;
                    }
                    switch (next) {
                        case '\\':
                            break;
                        case 'b':
                            chr = '\b';
                            break;
                        case 'f':
                            chr = '\f';
                            break;
                        case 'n':
                            chr = '\n';
                            break;
                        case 'r':
                            chr = '\r';
                            break;
                        case 't':
                            chr = '\t';
                            break;
                        case '\"':
                            chr = '\"';
                            break;
                        case '\'':
                            chr = '\'';
                            break;
                        case 'u':
                            if (idx >= str.length() - 5) {
                                chr = 'u';
                                break;
                            }
                            unescaped.append(
                                Character.toChars(
                                    Integer.parseInt(
                                        String.join(
                                            "",
                                            String.valueOf(str.charAt(idx + 2)),
                                            String.valueOf(str.charAt(idx + 3)),
                                            String.valueOf(str.charAt(idx + 4)),
                                            String.valueOf(str.charAt(idx + 5))
                                        ),
                                        16
                                    )
                                )
                            );
                            idx += 5;
                            continue;
                        default:
                            break;
                    }
                    ++idx;
                }
                unescaped.append(chr);
            }
            return unescaped.toString();
        }
    }

    /**
     * A single value as {@code Phi}.
     *
     * @param <T> The type of data
     * @since 0.1
     */
    final class Value<T> extends PhDefault implements Data<T> {

        /**
         * Value.
         */
        private final T val;

        /**
         * Ctor.
         * @param value Value
         */
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
                txt = String.format("array[%d]", ((Object[]) this.val).length);
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
