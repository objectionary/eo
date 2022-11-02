package org.eolang.maven;

import java.util.Collections;
import org.apache.maven.model.Dependency;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class DcsNoOneHasTransitiveTest {

    @Test
    void checksAllDependenciesWithoutTransitive() throws Exception {
        MatcherAssert.assertThat(
            new LengthOf(
                new DcsNoOneHasTransitive(single("eo-collections"), d -> empty())).value(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void checksAllDependenciesWithTransitive() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new DcsNoOneHasTransitive(single("eo-foo"), d -> single("eo-bar"))::iterator
        );
    }

    private Dependencies empty() {
        return Collections::emptyIterator;
    }

    private Dependencies single(final String artifact) {
        return () -> Collections.singletonList(dependency(artifact)).iterator();
    }

    private Dependency dependency(final String artifact) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId("org.eolang");
        dependency.setArtifactId(artifact);
        dependency.setVersion("0.1.0");
        dependency.setScope("compile");
        return dependency;
    }
}