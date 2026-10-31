/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;

/**
 * The class for emulating of Maven Central repository.
 * DummyCentral creates an empty dependency jar file under the path.
 *
 * @since 0.28.11
 */
final class DummyCentral implements BiConsumer<Dependency, Path> {

    @Override
    public void accept(final Dependency dependency, final Path path) {
        try {
            Files.createDirectories(path);
            Files.createFile(path.resolve(DummyCentral.className(dependency)));
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't save '%s' to '%s'", dependency, path),
                ex
            );
        }
    }

    private static String className(final Dependency dependency) {
        final List<String> parts = new ArrayList<>(3);
        if (dependency.getArtifactId() != null && !dependency.getArtifactId().isEmpty()) {
            parts.add(dependency.getArtifactId());
        }
        if (dependency.getVersion() != null && !dependency.getVersion().isEmpty()) {
            parts.add(dependency.getVersion());
        }
        if (dependency.getClassifier() != null && !dependency.getClassifier().isEmpty()) {
            parts.add(dependency.getClassifier());
        }
        return String.format("%s.class", String.join("-", parts));
    }
}
