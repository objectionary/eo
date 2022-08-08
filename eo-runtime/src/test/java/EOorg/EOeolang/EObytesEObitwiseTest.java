/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package EOorg.EOeolang;

import java.math.BigInteger;
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EObytes}.
 *
 * Bitwise methods.
 *
 * @since 0.23
 */
public final class EObytesEObitwiseTest {

    @ParameterizedTest
    @CsvSource({
        "0x0000",
        "0xFFFF",
    })
    public void bitwiseNot(
        final long first
    ) {
        final Phi left = new EOint$EOas_bytes(new Data.ToPhi(first));
        final Phi phi = new PhMethod(new PhCopy(left), "not");
        final byte[] expected = BigInteger.valueOf(first).not().toByteArray();
        MatcherAssert.assertThat(
            String.format("~ %08x != %s", first, Arrays.toString(expected)),
            new Dataized(phi.copy()).take(byte[].class),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "0xFF,     xor,  0xAA, as-bytes",
        "0xFF,     and,  0xAA, as-bytes",
        "0xFF,      or,  0xAA, as-bytes",
        "0x00,      or,  0xFF, as-bytes",
        "0x00,     xor,  0xAA, as-bytes",
    })
    public void bitwiseWorksOnBytes(
        final long first,
        final String method,
        final long second,
        final String type
    ) {
        final Phi left = new EOint$EOas_bytes(new Data.ToPhi(first));
        final Phi right = new Data.ToPhi(second);
        final Phi param = type == null
            ? new PhCopy(right)
            : new PhMethod(right, type);
        final Phi phi = new PhMethod(
            new PhWith(
                new PhMethod(new PhCopy(left), method),
                0,
                param
            ),
            "as-int"
        );
        final long expected;
        switch (method) {
            case "xor":
                expected = first ^ second;
                break;
            case "or":
                expected = first | second;
                break;
            case "and":
                expected = first & second;
                break;
            default:
                throw new UnsupportedOperationException(method);
        }
        MatcherAssert.assertThat(
            String.format("%08x %s %08x is not %08x", first, method, second, expected),
            new Dataized(phi.copy()).take(Long.class),
            Matchers.equalTo(expected)
        );
    }
}