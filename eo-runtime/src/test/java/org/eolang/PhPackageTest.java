/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
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

    @Test
    void copiesObject() {
        MatcherAssert.assertThat(
            "Every take() should return new instance, but it didn't",
            Phi.Φ.take("seq"),
            Matchers.not(
                Matchers.equalTo(
                    Phi.Φ.take("seq")
                )
            )
        );
    }

    @Test
    void doesNotSetRhoToGlobalObject() {
        Assertions.assertThrows(
            ExUnset.class,
            () -> Phi.Φ.take(Phi.RHO),
            String.format(
                "Global object '%s' must not have %s attribute",
                PhPackage.GLOBAL, Phi.RHO
            )
        );
    }

    @Test
    void setsRhoToObject() {
        final Phi pckg = Phi.Φ.take("fs");
        MatcherAssert.assertThat(
            String.format(
                "The %s attribute must be set to package object on dispatch",
                Phi.RHO
            ),
            pckg.take("dir").take(Phi.RHO),
            Matchers.equalTo(pckg)
        );
    }

    @Test
    void findsLongClass() {
        MatcherAssert.assertThat(
            "Package should resolve class with '$' in the name, but it didn't",
            Phi.Φ.take("bytes$eq").copy(),
            Matchers.instanceOf(Phi.class)
        );
    }

    @ParameterizedTest
    @MethodSource("attributes")
    void retrievesAttribute(final String attribute, final Class<?> expected) {
        MatcherAssert.assertThat(
            String.format(
                "Attribute '%s' should be instance of %s, but it wasn't",
                attribute, expected.getSimpleName()
            ),
            Phi.Φ.take(attribute),
            Matchers.instanceOf(expected)
        );
    }

    @Test
    void throwsExceptionIfCantInstantiateObject() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Phi.Φ.take("failed"),
            "Should throw if object cannot be instantiated, but it was"
        );
    }

    @Test
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void throwsExceptionIfCantFindPackageInfo() {
        MatcherAssert.assertThat(
            "Exception message must mention missing package-info.class",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("test.package-info"),
                "We should throw if package-info.class is missing"
            ).getMessage(),
            Matchers.equalTo(
                "Couldn't find object 'Φ.test' because there's no class 'org.eolang.EOtest' or package-info class: 'org.eolang.EOtest.package-info', at least one of them must exist"
            )
        );
    }

    @Test
    void returnsSelfOnCopy() {
        final Phi pckg = Phi.Φ;
        MatcherAssert.assertThat(
            "Package object should return itself on copying",
            pckg.copy(),
            Matchers.is(pckg)
        );
    }

    @Test
    void returnsForma() {
        MatcherAssert.assertThat(
            "Should return valid forma",
            Phi.Φ.forma(),
            Matchers.equalTo(PhPackage.GLOBAL)
        );
    }

    @Test
    void returnsLocator() {
        MatcherAssert.assertThat(
            "locator of the DEFAULT_PACKAGE must be ?:?:?, but is wasn't",
            Phi.Φ.locator(),
            Matchers.equalTo("?:?:?")
        );
    }

    @Test
    void findsAttributesInThreads() {
        final Phi pckg = Phi.Φ;
        MatcherAssert.assertThat(
            "Should take an attribute in multiple threads",
            new Together<>(
                thread -> pckg.take("go") instanceof PhPackage
            ),
            Matchers.allOf(
                Matchers.not(Matchers.hasItem(true))
            )
        );
    }

    private static Stream<Arguments> attributes() {
        return Stream.of(
            Arguments.of("bytes$eq", EObytes$EOeq.class),
            Arguments.of("go", EOgo.class)
        );
    }
}
