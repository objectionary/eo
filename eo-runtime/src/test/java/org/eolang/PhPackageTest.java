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

import EOorg.EOeolang.EObool$EOand;
import EOorg.EOeolang.EObytes$EOas_int;
import EOorg.EOeolang.EObytes$EOeq;
import EOorg.EOeolang.EOgoto;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link PhPackage}.
 *
 * @since 0.24
 */
final class PhPackageTest {

    /**
     * Default test package.
     */
    private static final String DEFAULT_PACKAGE = "org.eolang";

    @Test
    void takesPackage() {
        MatcherAssert.assertThat(
            Phi.Φ.attr("org").get().attr("eolang").get().attr("seq").get(),
            Matchers.equalTo(
                Phi.Φ.attr("org").get().attr("eolang").get().attr("seq").get()
            )
        );
    }

    @Test
    void findsLongClass() {
        MatcherAssert.assertThat(
            Phi.Φ.attr("org").get()
                .attr("eolang").get()
                .attr("bytes$eq").get().copy(),
            Matchers.instanceOf(Phi.class)
        );
    }

    @ParameterizedTest
    @MethodSource("attributes")
    void retrievesAttribute(final String attribute, final Class<?> expected) {
        final Phi parent = new PhPackage(PhPackageTest.DEFAULT_PACKAGE);
        final Phi actual = parent.attr(attribute).get();
        MatcherAssert.assertThat(
            actual,
            Matchers.instanceOf(expected)
        );
        if (!(actual instanceof PhPackage)) {
            MatcherAssert.assertThat(
                actual.attr("ρ").get(),
                Matchers.equalTo(parent)
            );
        }
    }

    @Test
    void throwsExceptionIfCantInstantiateObject() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).attr("failed").get()
        );
    }

    @Test
    void doesNotCopies() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).copy()
        );
    }

    @Test
    void doesNotGetAttributeByPosition() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).attr(0)
        );
    }

    @Test
    void doesNotGetForma() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).forma()
        );
    }

    @Test
    void convertsToPhiTerm() {
        MatcherAssert.assertThat(
            new PhPackage(PhPackageTest.DEFAULT_PACKAGE).φTerm(),
            Matchers.equalTo("Φ.org.eolang")
        );
    }

    @Test
    void returnsLocator() {
        MatcherAssert.assertThat(
            new PhPackage(PhPackageTest.DEFAULT_PACKAGE).locator(),
            Matchers.equalTo("?:?")
        );
    }

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            new PhPackage(PhPackageTest.DEFAULT_PACKAGE).toString(),
            Matchers.equalTo("Φ.org.eolang")
        );
    }

    @Test
    void findsAttributesConcurrently() throws InterruptedException {
        final int threads = Runtime.getRuntime().availableProcessors() + 10;
        final ExecutorService service = Executors.newFixedThreadPool(threads);
        final PhPackage pckg = new PhPackage(PhPackageTest.DEFAULT_PACKAGE);
        final Set<Integer> basket = Collections.synchronizedSet(new HashSet<>(threads));
        final CountDownLatch latch = new CountDownLatch(1);
        Stream.generate(
            () -> (Runnable) () -> {
                try {
                    latch.await();
                    basket.add(System.identityHashCode(pckg.attr("goto").get()));
                } catch (final InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(
                        "The testing thread was interrupted, current basket size %d",
                        exception
                    );
                }
            }
        ).limit(threads).forEach(service::submit);
        latch.countDown();
        service.shutdown();
        if (service.awaitTermination(1, TimeUnit.SECONDS)) {
            MatcherAssert.assertThat(basket.size(), Matchers.equalTo(threads));
        } else {
            throw new IllegalStateException(
                String.format(
                    "Failed to wait for threads to finish. Current basket size %d, but expected %d",
                    basket.size(),
                    threads
                )
            );
        }
    }

    private static Stream<Arguments> attributes() {
        return Stream.of(
            Arguments.of("absent", PhPackage.class),
            Arguments.of("bytes$as-int", EObytes$EOas_int.class),
            Arguments.of("bytes$eq", EObytes$EOeq.class),
            Arguments.of("goto", EOgoto.class),
            Arguments.of("bool$and", EObool$EOand.class)
        );
    }
}
