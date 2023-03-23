package org.eolang.maven.dependencies;

import java.util.Collections;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link DcsEachWithoutTransitive}.
 *
 * @since 0.30
 */
class DcsEachWithoutTransitiveTest {

    @Test
    void failsIfHasTransitiveDependencies() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new DcsEachWithoutTransitive(
                new DcsFake(),
                (dep) -> new DcsFake(100)
            ).iterator().next()
        );
    }

    @Test
    void keepsDependenciesThatHaveTestDependenciesAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                (dep) -> Collections.singleton(DcsFake.randDep("test"))
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasTheSameDependencyAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                DcsFake::new
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasRuntimeDependencyAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                (dep) -> Collections.singleton(DcsFake.runtime())
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    /**
     * Matcher for checking that iterable has all items from another iterable.
     * @param deps Iterable of dependencies
     * @return Matcher
     */
    private static Matcher<? super Iterable<Dependency>> hasAll(
        final Iterable<? extends Dependency> deps
    ) {
        return Matchers.allOf(
            new ListOf<>(deps)
                .stream()
                .map(Matchers::hasItem)
                .collect(Collectors.toList())
        );
    }
}