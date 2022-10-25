package org.eolang.maven.dependencies;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
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
}
