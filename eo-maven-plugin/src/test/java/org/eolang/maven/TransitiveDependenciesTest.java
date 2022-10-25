package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.model.Dependency;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class TransitiveDependenciesTest {

    @Test
    void hasDependencies(@TempDir final Path tmp) {
        final Path saved = saveFailedDependencyJson(tmp);
        final TransitiveDependencies dependencies = new TransitiveDependencies(
            new DummyDependencyTree(saved),
            saved
        );
        final boolean res = dependencies.hasDependencies(dependency());
        MatcherAssert.assertThat(res, Matchers.is(true));
    }

    @Test
    void hasNotDependencies(@TempDir final Path tmp) {
        final Path saved = saveDependencyJson(tmp);
        final TransitiveDependencies dependencies = new TransitiveDependencies(
            new DummyDependencyTree(saved),
            saved
        );
        final boolean res = dependencies.hasDependencies(dependency());
        MatcherAssert.assertThat(res, Matchers.is(false));
    }

    private Dependency dependency() {
        final Dependency dependency = new Dependency();
        dependency.setVersion("0.2.3");
        dependency.setArtifactId("eo-math");
        dependency.setGroupId("org.eolang");
        return dependency;
    }

    private Path saveFailedDependencyJson(final Path tmp) {
        try {
            final Path res = tmp.resolve("eo-math-dependencies-without-foreign.json");
            new Home().save(
                new ResourceOf(
                    "org/eolang/maven/dependencies/eo-math-dependencies-transient-dependency.json"),
                res
            );
            return res;
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private Path saveDependencyJson(final Path tmp) {
        try {
            final Path res = tmp.resolve("eo-math-dependencies-without-foreign.json");
            new Home().save(
                new ResourceOf(
                    "org/eolang/maven/dependencies/eo-math-dependencies-without-foreign.json"),
                res
            );
            return res;
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }
}