package org.eolang.maven.dependencies;

import java.util.function.Predicate;
import org.apache.maven.model.Dependency;

public class NoRuntimeDependency implements Predicate<Dependency> {
    @Override
    public boolean test(final Dependency dependency) {
        return !(dependency.getGroupId().equals("org.eolang") &&
                     dependency.getArtifactId().equals("eo-runtime"));
    }
}
