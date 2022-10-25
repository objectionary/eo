package org.eolang.maven;

import java.nio.file.Path;
import org.apache.maven.model.Dependency;

public interface DependencyTree {

    Path save(Dependency origin, Path dir);
}
