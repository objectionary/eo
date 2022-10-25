package org.eolang.maven.dependencies;

import java.util.List;
import org.apache.maven.model.Dependency;

public interface Dependencies {

    List<Dependency> toList();
}
