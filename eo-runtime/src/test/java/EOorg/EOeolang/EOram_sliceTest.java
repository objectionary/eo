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
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * Test case for {@link EOram$EOram_slice}.
 *
 * @since 0.23
 */
public final class EOram_sliceTest {

    @ParameterizedTest
    @CsvSource({
        "5,  0, hello, 0, 5, hello",
        "10, 5, hello, 5, 5, hello",
        "13, 0, hello world, 6, 5, world"
    })
    void ramSlice(
        final long total,
        final int wrt,
        final String data,
        final int rdr,
        final int len,
        final String result
    ) throws IOException {
        final Phi ref = new PhWith(new EOram(Phi.Φ), 0, new Data.ToPhi(total));
        Ram.INSTANCE.write(ref, wrt, data.getBytes(StandardCharsets.UTF_8));
        final Phi slice = new PhMethod(ref, "slice");
        final Phi phi = new PhWith(
            new PhWith(
                slice,
                "position",
                new Data.ToPhi((long) rdr)
            ),
            "size",
            new Data.ToPhi((long) len)
        );

        final byte[] bytes = new Dataized(phi).take(byte[].class);
        MatcherAssert.assertThat(
            new String(bytes, StandardCharsets.UTF_8),
            Matchers.is(
                result
            )
        );
    }

}
