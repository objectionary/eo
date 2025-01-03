/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import EOorg.EOeolang.EObytes$EOas_number;
import EOorg.EOeolang.EObytes$EOeq;
import EOorg.EOeolang.EOgo;
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
@SuppressWarnings("PMD.TooManyMethods")
final class PhPackageTest {

    /**
     * Default test package.
     */
    private static final String DEFAULT_PACKAGE = "org.eolang";

    @Test
    void copiesObject() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            Phi.Φ.take("org").take("eolang").take("seq"),
            Matchers.not(
                Matchers.equalTo(
                    Phi.Φ.take("org").take("eolang").take("seq")
                )
            )
        );
    }

    @Test
    void setsRhoToObject() {
        final Phi eolang = Phi.Φ.take("org").take("eolang");
        final Phi seq = eolang.take("seq");
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            seq.take(Attr.RHO),
            Matchers.equalTo(eolang)
        );
    }

    @Test
    void findsLongClass() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            Phi.Φ.take("org")
                .take("eolang")
                .take("bytes$eq").copy(),
            Matchers.instanceOf(Phi.class)
        );
    }

    @ParameterizedTest
    @MethodSource("attributes")
    void retrievesAttribute(final String attribute, final Class<?> expected) {
        final Phi parent = new PhPackage(PhPackageTest.DEFAULT_PACKAGE);
        final Phi actual = parent.take(attribute);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.instanceOf(expected)
        );
    }

    @Test
    void throwsExceptionIfCantInstantiateObject() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).take("failed"),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void doesNotCopies() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).copy(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void doesNotGetForma() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhPackage(PhPackageTest.DEFAULT_PACKAGE).forma(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void returnsLocator() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new PhPackage(PhPackageTest.DEFAULT_PACKAGE).locator(),
            Matchers.equalTo("?:?")
        );
    }

    @Test
    @SuppressWarnings("PMD.CloseResource")
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
                    basket.add(System.identityHashCode(pckg.take("go")));
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
            MatcherAssert.assertThat(
                AtCompositeTest.TO_ADD_MESSAGE,
                basket.size(),
                Matchers.equalTo(threads)
            );
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
            Arguments.of("bytes$as-number", EObytes$EOas_number.class),
            Arguments.of("bytes$eq", EObytes$EOeq.class),
            Arguments.of("go", EOgo.class)
        );
    }
}
