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

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.concurrent.ConcurrentSkipListSet;
import org.eolang.Data;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.Ram;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.EnabledIf;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link Ram}.
 * @since 1.0
 * @checkstyle ParameterNumberCheck (17 lines)
 */
@Execution(ExecutionMode.CONCURRENT)
@DisabledOnOs(OS.MAC)
@Disabled
final class RamTest {
    @ParameterizedTest
    @CsvSource({
        "5,  0, hello, 0, 5, hello",
        "10, 5, hello, 5, 5, hello",
        "13, 0, hello world, 6, 5, world"
    })
    void writesAndReads(
        final long total,
        final int wrt,
        final String data,
        final int rdr,
        final int len,
        final String result
    ) throws IOException {
        final Phi ref = new PhWith(
            new EOram(Phi.Φ),
            0,
            new Data.ToPhi(total)
        );
        Ram.INSTANCE.write(ref, wrt, data.getBytes(StandardCharsets.UTF_8));
        final byte[] bytes = Ram.INSTANCE.read(ref, rdr, len);
        MatcherAssert.assertThat(
            new String(bytes, StandardCharsets.UTF_8),
            Matchers.is(result)
        );
    }

    /**
     * Is this system multi-core?
     * @return Boolean.
     */
    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static boolean isMulticore() {
        return Runtime.getRuntime().availableProcessors() > 2;
    }

    /**
     * Test for concurrent access.
     * @since 1.0
     */
    @Nested
    @Execution(ExecutionMode.CONCURRENT)
    @TestInstance(TestInstance.Lifecycle.PER_CLASS)
    @EnabledIf("EOorg.EOeolang.RamTest#isMulticore")
    final class ConcurrentAccessTheToSameDataTest {
        /**
         * Ram object reference.
         */
        private final Phi ref = new PhWith(
            new EOram(Phi.Φ),
            0,
            new Data.ToPhi(128L)
        );

        /**
         * Used threads.
         */
        private final Collection<Long> threads = new ConcurrentSkipListSet<>();

        @ParameterizedTest
        @CsvSource({
            "hello       ,  0",
            "bye         , 10",
            "hello world , 20",
            "привет      , 50"
        })
        void writesAndReads(
            final String data,
            final int position
        ) throws IOException {
            this.threads.add(Thread.currentThread().getId());
            Ram.INSTANCE.write(this.ref, position, data.getBytes(StandardCharsets.UTF_8));
            MatcherAssert.assertThat(
                new String(
                    Ram.INSTANCE.read(
                        this.ref,
                        position,
                        data.getBytes(StandardCharsets.UTF_8).length
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.is(data)
            );
        }

        @AfterAll
        void shouldBeConcurrent() {
            MatcherAssert.assertThat(
                "test was executed concurrently",
                this.threads,
                Matchers.hasSize(Matchers.greaterThan(1))
            );
        }
    }

}
