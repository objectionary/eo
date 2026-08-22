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
 * @since 0.24
 */
final class PhPackageTest {

    @Test
    void printsGlobalScopeAsTerm() {
        MatcherAssert.assertThat(
            "Global package must render its name as φ-term, but it didnt",
            Phi.Φ.φTerm(),
            Matchers.equalTo("Φ")
        );
    }

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
    void reportsNoRhoOnGlobalObject() {
        MatcherAssert.assertThat(
            "hasRho() must agree with take(RHO) and report the global object as lacking ρ",
            Phi.Φ.hasRho(),
            Matchers.is(false)
        );
    }

    @Test
    void reportsRhoOnceBound() {
        final Phi pckg = new PhPackage("test-rho");
        pckg.put(Phi.RHO, Phi.Φ);
        MatcherAssert.assertThat(
            "hasRho() must turn true as soon as ρ is bound into the package",
            pckg.hasRho(),
            Matchers.is(true)
        );
    }

    @Test
    void setsRhoToObject() {
        final Phi pckg = Phi.Φ;
        MatcherAssert.assertThat(
            String.format(
                "The %s attribute must be set to package object on dispatch",
                Phi.RHO
            ),
            pckg.take("nop").take(Phi.RHO),
            Matchers.equalTo(pckg)
        );
    }

    @Test
    void hasRhoReportsAbsenceOnGlobalObject() {
        MatcherAssert.assertThat(
            String.format(
                "hasRho() must agree with take(%s) and report the global object as unset",
                Phi.RHO
            ),
            Phi.Φ.hasRho(),
            Matchers.is(false)
        );
    }

    @Test
    void hasRhoReportsPresenceAfterDispatch() {
        MatcherAssert.assertThat(
            String.format(
                "hasRho() must report %s as set once dispatch has bound it",
                Phi.RHO
            ),
            Phi.Φ.take("nop").hasRho(),
            Matchers.is(true)
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
    void throwsExceptionIfCantFindPackageInfo() {
        MatcherAssert.assertThat(
            "Exception message must mention missing package-info.class and suggest a close object",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("test.package-info"),
                "We should throw if package-info.class is missing"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString(
                    "Couldn't find object 'Φ.test' because there's no class 'org.eolang.EOtest' or package-info class: 'org.eolang.EO_test.package-info', at least one of them must exist"
                ),
                Matchers.containsString("Did you mean?")
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
        MatcherAssert.assertThat(
            "Should take an attribute in multiple threads",
            new Together<>(
                thread -> Phi.Φ.take("nop") instanceof PhPackage
            ),
            Matchers.allOf(
                Matchers.not(Matchers.hasItem(true))
            )
        );
    }

    private static Stream<Arguments> attributes() {
        return Stream.of(
            Arguments.of("bytes$eq", EObytes$EOeq.class),
            Arguments.of("nop", EOnop.class)
        );
    }
}
