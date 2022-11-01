/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
    public void accept(
        final Dependency dependency,
        final Path path
    ) {
        try {
            Files.createDirectories(path);
            final String other = DummyCentral.jarName(dependency);
            Files.createFile(path.resolve(other));
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't save '%s' to '%s'", dependency, path),
                ex
            );
        }
    }

    /**
     * Dependency jar name.
     *
     * @param dependency Dependency
     * @return Jar file name
     */
    private static String jarName(final Dependency dependency) {
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
        return String.format("%s.jar", String.join("-", parts));
    }
}
