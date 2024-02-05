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

/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOint}.
 *
 * @since 0.1
 * @checkstyle TypeNameCheck (4 lines)
 */
public final class EOintTest {

    @Test
    void hasEqualHashes() {
        final Phi left = new Data.ToPhi(42L);
        final Phi right = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            left.hashCode(),
            Matchers.equalTo(right.hashCode())
        );
    }

    @Test
    void hasHashEvenWithoutData() {
        final Phi phi = new EOint(Phi.Φ);
        MatcherAssert.assertThat(
            phi.hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void hasDifferentHash() {
        final Phi raw = new EOint(Phi.Φ);
        final Phi initialized = new Data.ToPhi(0L);
        MatcherAssert.assertThat(
            raw.hashCode(),
            Matchers.not(initialized.hashCode())
        );
    }
}
