package org.eolang.maven;

import java.nio.file.Path;
import org.apache.maven.model.Dependency;

public class DummyDependencyTree implements DependencyTree {

    private final Path saved;

    public DummyDependencyTree(final Path saved) {
        this.saved = saved;
    }

    @Override
    public Path save(
        final Dependency ignore,
        final Path ignored
    ) {
        return saved;
    }
}
