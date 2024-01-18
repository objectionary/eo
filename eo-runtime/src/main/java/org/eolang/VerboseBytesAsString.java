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
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Makes a String from byte array that can represent bool,
 *  double, int or {@link String}.
 *
 * @since 0.36
 */
public final class VerboseBytesAsString implements Supplier<String> {

    private final byte[] data;

    public VerboseBytesAsString(byte[] data) {
        this.data = data;
    }

    @Override
    public String get() {
        final Bytes bytes = new BytesOf(data);
        final String result;
        switch (data.length) {
            case 0:
                result = String.format(
                    "%s",
                    Arrays.toString(data)
                );
                break;
            case 1:
                result = String.format(
                    "%s = %s",
                    Arrays.toString(data),
                    data[0] != 0
                );
                break;
            case 8:
                result = String.format(
                    "%s = %s, or %s, or \"%s\")",
                    Arrays.toString(data),
                    bytes.asNumber(Long.class),
                    bytes.asNumber(Double.class),
                    new String(data, StandardCharsets.UTF_8)
                );
                break;
            default:
                result = String.format(
                    "%s = \"%s\"",
                    Arrays.toString(data),
                    new String(data, StandardCharsets.UTF_8)
                );
        }
        return result;
    }

}
