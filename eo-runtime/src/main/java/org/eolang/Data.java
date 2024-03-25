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

import java.nio.charset.StandardCharsets;

/**
 * A data container.
 *
 * @since 0.1
 */
@Versionized
public interface Data {

    /**
     * Take the data.
     * @return The data
     */
    byte[] data();

    /**
     * Makes a {@link Phi} out of a Java object, like {@link String} or {@link Integer}.
     *
     * @since 0.1
     */
    final class ToPhi extends PhEnvelope {
        /**
         * Ctor.
         * @param obj Data
         */
        public ToPhi(final Object obj) {
            super(Data.ToPhi.toPhi(obj));
        }

        /**
         * Convert to Phi object.
         * @param obj Object to convert
         * @return Constructed Phi
         */
        private static Phi toPhi(final Object obj) {
            final Phi object;
            byte[] bytes;
            final boolean delta;
            final Phi eolang = Phi.Φ.attr("org").get().attr("eolang").get();
            if (obj instanceof Boolean) {
                object = eolang.attr("bool").get().copy();
                delta = false;
                if (obj.equals(true)) {
                    bytes = new byte[] {0x01};
                } else {
                    bytes = new byte[] {0x00};
                }
            } else if (obj instanceof byte[]) {
                object = eolang.attr("bytes").get().copy();
                delta = true;
                bytes = (byte[]) obj;
            } else if (obj instanceof Long) {
                object = eolang.attr("int").get().copy();
                delta = false;
                bytes = new BytesOf((Long) obj).data();
            } else if (obj instanceof String) {
                object = eolang.attr("string").get().copy();
                delta = false;
                bytes = Data.ToPhi.unescapeJavaString(
                    (String) obj
                ).getBytes(StandardCharsets.UTF_8);
            } else if (obj instanceof Double) {
                object = eolang.attr("float").get().copy();
                delta = false;
                bytes = new BytesOf((Double) obj).data();
            } else {
                throw new IllegalArgumentException(
                    String.format(
                        "Unknown type of data: %s",
                        obj.getClass().getCanonicalName()
                    )
                );
            }
            final Phi phi;
            if (delta) {
                phi = new PhData(object, bytes);
            } else {
                object.attr(0).put(new PhData(eolang.attr("bytes").get().copy(), bytes));
                phi = object;
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
}
