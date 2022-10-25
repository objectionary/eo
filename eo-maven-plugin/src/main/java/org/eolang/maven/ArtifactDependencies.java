package org.eolang.maven;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import org.apache.maven.model.Dependency;

public class ArtifactDependencies implements Dependencies {

    private final Dependencies dependencies;

    public ArtifactDependencies(final Path file, final Dependency dependency) {
        this(new FilteredDependencies(
            new JsonDependencies(file),
            Arrays.asList(
                new NoRuntimeDependency(),
                new NoSameDependency(dependency),
                new NoTestDependency()
            )
        ));
    }

    public ArtifactDependencies(final Dependencies dependencies) {
        this.dependencies = dependencies;
    }

    @Override
    public List<Dependency> toList() {
        return dependencies.toList();
    }

    public static class NoRuntimeDependency implements Predicate<Dependency> {
        @Override
        public boolean test(final Dependency dependency) {
            return !(dependency.getGroupId().equals("org.eolang") &&
                         dependency.getArtifactId().equals("eo-runtime"));
        }
    }

    public static class NoSameDependency implements Predicate<Dependency> {

        private final Dependency current;

        public NoSameDependency(final Dependency current) {
            this.current = current;
        }

        @Override
        public boolean test(final Dependency dependency) {
            return !(dependency.getGroupId().equals(current.getGroupId()) &&
                         dependency.getArtifactId().equals(current.getArtifactId()));
        }
    }

    public static class NoTestDependency implements Predicate<Dependency> {
        @Override
        public boolean test(final Dependency dependency) {
            final String scope = dependency.getScope();
            return !scope.contains("test");
        }
    }
}
