package org.eolang.maven.dependencies;

import java.util.function.Predicate;
import org.apache.maven.model.Dependency;

public class NoTestDependency implements Predicate<Dependency> {
    @Override
    public boolean test(final Dependency dependency) {
        final String scope = dependency.getScope();
        return !scope.contains("test");
    }
}
