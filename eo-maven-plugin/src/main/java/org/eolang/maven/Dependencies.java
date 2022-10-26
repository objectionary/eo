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

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import org.apache.maven.model.Dependency;

/**
 * Maven dependencies abstraction.
 *
 * @since 0.28.11
 */
interface Dependencies {

    /**
     * Converts to a plain list.
     *
     * @return List of Maven Dependencies
     */
    Collection<Dependency> toList();

    /**
     * Filtered dependencies.
     *
     * @since 0.28.11
     */
    class DcsFiltered implements Dependencies {

        /**
         * Decorated.
         */
        private final Dependencies dependencies;

        /**
         * All filters.
         */
        private final Collection<Predicate<Dependency>> filters;

        /**
         * The main constructor.
         *
         * @param dependencies Decorated
         * @param filters All fiters
         */
        DcsFiltered(
            final Dependencies dependencies,
            final Collection<Predicate<Dependency>> filters
        ) {
            this.dependencies = dependencies;
            this.filters = filters;
        }

        @Override
        public Collection<Dependency> toList() {
            return this.dependencies.toList()
                .stream()
                .filter(this::filter)
                .collect(Collectors.toList());
        }

        /**
         * Apply all filters for Dependency.
         *
         * @param dependency Dependency
         * @return True if all filters were passed
         */
        private boolean filter(final Dependency dependency) {
            return this.filters.stream().allMatch(f -> f.test(dependency));
        }
    }

    /**
     * Dependencies uploaded from json file.
     *
     * @since 0.28.11
     */
    class DcsJson implements Dependencies {

        /**
         * File path.
         */
        private final Path file;

        /**
         * The main constructor.
         *
         * @param file File path
         */
        DcsJson(final Path file) {
            this.file = file;
        }

        @Override
        public Collection<Dependency> toList() {
            try {
                final List<Dependency> all = new ArrayList<>(0);
                if (Files.exists(this.file)) {
                    Logger.debug(this, String.format("Dependencies file: %s", this.file));
                    final JsonReader reader = Json.createReader(Files.newBufferedReader(this.file));
                    final JsonArray artifacts = reader.readObject()
                        .getJsonArray("artifacts");
                    for (final JsonValue artifact : artifacts) {
                        final JsonObject obj = artifact.asJsonObject();
                        final String group = obj.getString("groupId");
                        final String id = obj.getString("artifactId");
                        final String version = obj.getString("version");
                        final String scope = obj.getJsonArray("scopes").stream()
                            .map(JsonValue::toString)
                            .findFirst().orElseThrow(IllegalStateException::new);
                        final Dependency dependency = new Dependency();
                        dependency.setGroupId(group);
                        dependency.setArtifactId(id);
                        dependency.setVersion(version);
                        dependency.setScope(scope);
                        all.add(dependency);
                    }
                }
                return all;
            } catch (final IOException | IllegalStateException ex) {
                throw new IllegalStateException(
                    String.format(
                        "Exception happens during reading the dependencies from json file %s. %s",
                        this.file,
                        "Probably file is absent or you have a wrong json format"
                    ),
                    ex
                );
            }
        }
    }
}
