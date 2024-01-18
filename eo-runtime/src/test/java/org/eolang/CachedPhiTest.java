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

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CachedPhi}.
 *
 * @since 0.19
 */
final class CachedPhiTest {

    @Test
    void caches() {
        final AtomicInteger count = new AtomicInteger(0);
        final CachedPhi cphi = new CachedPhi();
        final Supplier<Phi> sup = () -> {
            count.incrementAndGet();
            return new Data.ToPhi(42L);
        };
        new Dataized(cphi.get("a", sup)).take(Long.class);
        new Dataized(cphi.get("b", sup)).take(Long.class);
        MatcherAssert.assertThat(
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void cachesRecursive() {
        final AtomicInteger count = new AtomicInteger(3);
        final CachedPhi cphi = new CachedPhi();
        final AtomicReference<Supplier<Phi>> sup = new AtomicReference<>();
        sup.set(
            () -> {
                final Phi result;
                if (count.decrementAndGet() == 0) {
                    result = new Data.ToPhi(42L);
                } else {
                    result = cphi.get("x", sup.get());
                }
                return result;
            }
        );
        MatcherAssert.assertThat(
            new Dataized(cphi.get("x", sup.get())).take(Long.class),
            Matchers.equalTo(42L)
        );
    }
}
