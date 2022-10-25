package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import org.apache.maven.model.Dependency;

public class TransitiveDependencies {

    private final DependencyTree tree;
    private final Path path;

    public TransitiveDependencies(
        final DependencyTree tree,
        final Path path
    ) {
        this.tree = tree;
        this.path = path;
    }

    boolean hasDependencies(final Dependency origin) {
        try {
            final JsonReader reader = Json.createReader(Files.newBufferedReader(path));
            final JsonArray artifacts = reader.readObject()
                .getJsonArray("artifacts");
            List<Dependency> all = new ArrayList<>();
            for (final JsonValue artifact : artifacts) {
                final JsonObject obj = artifact.asJsonObject();
                final String groupId = obj.getString("groupId");
                final String artifactId = obj.getString("artifactId");
                final String version = obj.getString("version");
                final String scope = obj.getJsonArray("scopes").stream().map(
                    JsonValue::toString).findFirst().orElseThrow(IllegalStateException::new);
                all.add(dependency(groupId, artifactId, version, scope));
            }
            List<Dependency> remains = filter(origin, all);
            return !remains.isEmpty();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private List<Dependency> filter(final Dependency original, final List<Dependency> all) {
        return all.stream()
            .filter(this::isNotRuntimeDependency)
            .filter(this::isRequiredScope)
            .filter(d -> isNotTheSameDependency(original, d))
            .collect(Collectors.toList());
    }

    private boolean isRequiredScope(final Dependency dependency) {
        final String scope = dependency.getScope();
        return !scope.contains("test");
    }

    private boolean isNotTheSameDependency(
        final Dependency origin,
        final Dependency dependency
    ) {
        return !(dependency.getGroupId().equals(origin.getGroupId()) &&
                     dependency.getArtifactId().equals(origin.getArtifactId()));
    }

    private boolean isNotRuntimeDependency(final Dependency dependency) {
        return !(dependency.getGroupId().equals("org.eolang") &&
                     dependency.getArtifactId().equals("eo-runtime"));
    }

    private static Dependency dependency(
        final String groupId,
        final String artifactId,
        final String version,
        final String scope
    ) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId(groupId);
        dependency.setArtifactId(artifactId);
        dependency.setVersion(version);
        dependency.setScope(scope);
        return dependency;
    }

}
