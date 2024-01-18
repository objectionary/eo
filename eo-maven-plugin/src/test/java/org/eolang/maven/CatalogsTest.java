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
package org.eolang.maven;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.nio.file.Path;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.number.SumOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Concurrency tests for {@link Catalogs}.
 * All tests in that class must be executed in parallel and in order to be sure that
 * everything works fine it's important to run the tests many times.
 * @since 0.29.0
 */
class CatalogsTest {

    /**
     * Number of cores on the running system.
     */
    private static final int CORES = Runtime.getRuntime().availableProcessors();

    @Test
    void readsFromTojosConcurrently(@TempDir final Path tmp) {
        final Tojos tojos = Catalogs.INSTANCE.make(tmp.resolve("foreign"), "json");
        MatcherAssert.assertThat(
            new SumOf(
                new Threads<>(
                    CatalogsTest.CORES,
                    IntStream.range(0, CatalogsTest.CORES)
                        .mapToObj(i -> tojos.add(UUID.randomUUID().toString()))
                        .map(CatalogsTest::task)
                        .collect(Collectors.toList())
                )
            ),
            Matchers.equalTo(CatalogsTest.CORES)
        );
    }

    /**
     * Task to be executed in parallel.
     * @param tojo Tojo
     * @return Scalar
     */
    private static Scalar<Integer> task(final Tojo tojo) {
        return () -> {
            final String uuid = "uuid";
            tojo.set(uuid, UUID.randomUUID().toString());
            tojo.get(uuid);
            tojo.get(uuid);
            tojo.get(uuid);
            tojo.get(uuid);
            return 1;
        };
    }
}
