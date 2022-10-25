package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.model.Dependency;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.dependencies.ArtifactDependencies;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class TransitiveDependenciesTest {

    @Test
    void hasDependencies(@TempDir final Path tmp) {
        final Path saved = saveFailedDependencyJson(tmp);
        final List<Dependency> all = new ArtifactDependencies(saved, dependency()).toList();
        MatcherAssert.assertThat(all.isEmpty(), Matchers.is(false));
    }

    @Test
    void hasNotDependencies(@TempDir final Path tmp) {
        final Path saved = saveDependencyJson(tmp);
        final List<Dependency> all = new ArtifactDependencies(saved, dependency()).toList();
        MatcherAssert.assertThat(all.isEmpty(), Matchers.is(true));
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
            final Path res = tmp.resolve("eo-math-dependencies-transient-dependency.json");
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