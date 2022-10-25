package org.eolang.maven.dependencies;

import java.util.function.Predicate;
import org.apache.maven.model.Dependency;

public class NoSameDependency implements Predicate<Dependency> {

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
