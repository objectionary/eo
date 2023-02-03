/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOarray}.
 *
 * @since 0.1
 */
public final class EOarrayEOatTest {

    @Test
    public void pushesAndGetsBack() {
        final String txt = "Hello, world!";
        final Phi str = new Data.ToPhi(txt);
        final Phi array = new Data.ToPhi(new Phi[] {str});
        final Phi idx = new Data.ToPhi(0L);
        final Phi get = array.attr("at").get();
        get.attr(0).put(idx);
        MatcherAssert.assertThat(
            new Dataized(get).take(String.class),
            Matchers.equalTo(txt)
        );
        MatcherAssert.assertThat(
            new Dataized(get).take(),
            Matchers.equalTo(txt)
        );
    }

    @Test
    public void checksNegativeIndex() {
        MatcherAssert.assertThat(
            new Dataized(this.get(-1L)).take(String.class),
            Matchers.equalTo("second")
        );
        MatcherAssert.assertThat(
            new Dataized(this.get(-2L)).take(String.class),
            Matchers.equalTo("first")
        );
    }

    @Test
    public void checksOutOfBounds() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(this.get(-3L)).take()
        );
    }

    private Phi get(final long index) {
        final String first = "first";
        final String second = "second";
        final Phi array = new Data.ToPhi(
            new Phi[] {
                new Data.ToPhi(first),
                new Data.ToPhi(second),
            });
        final Phi idx = new Data.ToPhi(index);
        final Phi get = array.attr("at").get();
        get.attr(0).put(idx);
        return get;
    }
}
