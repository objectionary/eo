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
package org.eolang.maven;

import java.util.concurrent.TimeUnit;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link org.eolang.maven.Timeout}.
 * @since 0.28.12
 */
final class TimeoutTest {

    @Test
    void interruptsIfTimeout() {
        final Timeout timeout = new Timeout(100, TimeUnit.MILLISECONDS);
        timeout.start();
        Assertions.assertThrows(InterruptedException.class, () -> Thread.sleep(150));
    }

    @Test
    void stopsSuccessfully() throws InterruptedException {
        final Timeout timeout = new Timeout(100, TimeUnit.MILLISECONDS);
        timeout.start();
        final boolean success = true;
        timeout.stop();
        Thread.sleep(150);
        MatcherAssert.assertThat(success, Matchers.is(true));
    }

    @Test
    void throwsExceptionDuringExecution() {
        try {
            final Timeout timeout = new Timeout(100, TimeUnit.MILLISECONDS);
            timeout.start();
            throw new IllegalStateException("Some occasion");
        } catch (final IllegalStateException ignore) {
            Assertions.assertThrows(InterruptedException.class, () -> Thread.sleep(150));
        }
    }
}
