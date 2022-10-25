package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.model.Dependency;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.dependencies.ArtifactDependencies;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ArtifactDependenciesTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, false",
        "eo-math-dependencies-without-foreign.json, true"

    })
    void selectsOnlyNonTestNonRuntimeNonSameDependencies(
        final String name,
        final boolean empty, @
        TempDir final Path tmp
    ) {
        final Path file = dependenciesJson(tmp, name);
        final List<Dependency> all = new ArtifactDependencies(file, sameDependency()).toList();
        MatcherAssert.assertThat(all.isEmpty(), Matchers.is(empty));
    }

    private Dependency sameDependency() {
        final Dependency dependency = new Dependency();
        dependency.setVersion("0.2.3");
        dependency.setArtifactId("eo-math");
        dependency.setGroupId("org.eolang");
        return dependency;
    }

    private Path dependenciesJson(final Path tmp, final String name) {
        try {
            final Path res = tmp.resolve(name);
            new Home().save(
                new ResourceOf(
                    String.format("org/eolang/maven/dependencies/%s", name)),
                res
            );
            return res;
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }
}