/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link PhClasspathObjects}.
 * @since 1.0
 */
@Isolated
final class PhClasspathObjectsTest {

    @Test
    void discoversObjectsFromArbitraryDependencyJars(
        @TempDir final Path temp
    ) throws IOException {
        final String first = PhClasspathObjectsTest.unique(temp, "first");
        final String second = PhClasspathObjectsTest.unique(temp, "second");
        try (
            PhClasspathObjectsTest.Classpath ignored = PhClasspathObjectsTest.replace(
                Arrays.asList(
                    PhClasspathObjectsTest.dependency(
                        temp, "first.jar", String.format("%s.printer", first)
                    ),
                    PhClasspathObjectsTest.dependency(
                        temp, "second.jar", String.format("%s.sink", second)
                    )
                )
            )
        ) {
            final PhSuggestions suggestions = new PhSuggestions();
            MatcherAssert.assertThat(
                "Default suggestions must be built from classpath JAR entries",
                Arrays.asList(
                    suggestions.suggestions(
                        String.format("Φ.%s.prnter", first),
                        5
                    ).get(0),
                    suggestions.suggestions(
                        String.format("Φ.%s.snk", second),
                        5
                    ).get(0)
                ),
                Matchers.contains(
                    String.format("%s.printer", first),
                    String.format("%s.sink", second)
                )
            );
        }
    }

    /**
     * Create dependency JAR with EO-style object resource.
     * @param dir Directory
     * @param name JAR file name
     * @param object Object part
     * @return JAR path
     * @throws IOException If JAR can't be written
     */
    private static Path dependency(
        final Path dir,
        final String name,
        final String object
    ) throws IOException {
        final Path jar = dir.resolve(name);
        try (JarOutputStream output = new JarOutputStream(Files.newOutputStream(jar))) {
            output.putNextEntry(
                new JarEntry(
                    String.format("org/eolang/EO%s.class", object.replace(".", "/EO"))
                )
            );
            output.closeEntry();
        }
        return jar;
    }

    /**
     * Unique EO package part based on JUnit temporary directory.
     * @param temp Temporary directory
     * @param prefix Prefix
     * @return Unique package part
     */
    private static String unique(final Path temp, final String prefix) {
        return String.format(
            "%s%s",
            prefix,
            temp.getFileName().toString().replaceAll("[^A-Za-z0-9]", "")
        );
    }

    /**
     * Replace Java classpath system property.
     * @param entries Classpath entries
     * @return Guard
     */
    private static PhClasspathObjectsTest.Classpath replace(final List<Path> entries) {
        return PhClasspathObjectsTest.replace(
            System.getProperty(PhClasspathObjectsTest.classpath()),
            PhClasspathObjectsTest.join(entries)
        );
    }

    /**
     * Replace Java classpath system property.
     * @param origin Original classpath
     * @param value New classpath
     * @return Guard
     */
    private static PhClasspathObjectsTest.Classpath replace(
        final String origin,
        final String value
    ) {
        System.setProperty(PhClasspathObjectsTest.classpath(), value);
        return new PhClasspathObjectsTest.Classpath(origin);
    }

    /**
     * Java classpath system property.
     * @return Property name
     */
    private static String classpath() {
        return "java.class.path";
    }

    /**
     * Join classpath entries.
     * @param entries Entries
     * @return Classpath
     */
    private static String join(final List<Path> entries) {
        return entries.stream()
            .map(Path::toString)
            .collect(Collectors.joining(File.pathSeparator));
    }

    /**
     * Classpath system property guard.
     * @since 1.0
     */
    private static final class Classpath implements AutoCloseable {

        /**
         * Original value.
         */
        private final String origin;

        /**
         * Ctor.
         * @param origin Original value
         */
        Classpath(final String origin) {
            this.origin = origin;
        }

        @Override
        public void close() {
            if (this.origin == null) {
                System.clearProperty(PhClasspathObjectsTest.classpath());
            } else {
                System.setProperty(PhClasspathObjectsTest.classpath(), this.origin);
            }
        }
    }
}
