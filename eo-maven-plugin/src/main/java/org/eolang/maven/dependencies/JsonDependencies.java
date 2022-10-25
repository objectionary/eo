package org.eolang.maven.dependencies;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import org.apache.maven.model.Dependency;

public class JsonDependencies implements Dependencies {

    private final Path file;

    public JsonDependencies(final Path file) {
        this.file = file;
    }

    @Override
    public List<Dependency> toList() {
        final JsonReader reader;
        try {
            reader = Json.createReader(Files.newBufferedReader(file));
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
            return all;
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
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
