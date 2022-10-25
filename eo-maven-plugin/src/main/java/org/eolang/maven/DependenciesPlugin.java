package org.eolang.maven;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.model.Dependency;

public interface DependenciesPlugin {

    Path dependenciesFile(final Dependency origin);

    class Dummy implements DependenciesPlugin {

        @Override
        public Path dependenciesFile(final Dependency origin) {
            return Paths.get("non-existent");
        }
    }
}
