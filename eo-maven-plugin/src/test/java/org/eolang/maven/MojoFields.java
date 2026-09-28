/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The names every mojo of the plugin declares as a field.
 *
 * <p>A parameter of {@link FakeMaven} whose name matches none of them is
 * dropped before the mojo ever sees it, so the mojo runs with its default and
 * the test passes while testing something else (#7268). Reading the names off
 * the classes themselves is what makes a rename break the test loudly.</p>
 *
 * <p>The classes are read from the directory the plugin's own are compiled
 * into. When that cannot be listed the answer is empty and nothing is
 * refused, since a test must not fail over the shape of somebody's
 * classpath.</p>
 *
 * @since 0.74.0
 */
final class MojoFields {

    /**
     * The prefix every mojo class name carries.
     */
    private final String prefix;

    /**
     * Ctor.
     */
    MojoFields() {
        this("Mj");
    }

    /**
     * Ctor.
     *
     * @param mark The prefix every mojo class name carries
     */
    MojoFields(final String mark) {
        this.prefix = mark;
    }

    /**
     * Every name declared by a mojo of the plugin.
     *
     * @return The names, empty when the mojos cannot be found
     */
    Set<String> all() {
        final Set<String> found = new HashSet<>();
        try {
            final Path dir = Paths.get(
                MjSafe.class.getProtectionDomain().getCodeSource().getLocation().toURI()
            ).resolve("org").resolve("eolang").resolve("maven");
            if (Files.isDirectory(dir)) {
                final Iterable<Path> files;
                try (Stream<Path> listed = Files.list(dir)) {
                    files = listed.collect(Collectors.toList());
                }
                for (final Path file : files) {
                    final String name = file.getFileName().toString();
                    if (name.startsWith(this.prefix) && name.endsWith(".class")) {
                        Class<?> walked = Class.forName(
                            String.format(
                                "org.eolang.maven.%s", name.substring(0, name.length() - 6)
                            )
                        );
                        while (walked != null) {
                            Stream.of(walked.getDeclaredFields())
                                .map(Field::getName)
                                .forEach(found::add);
                            walked = walked.getSuperclass();
                        }
                    }
                }
            }
        } catch (final URISyntaxException | IOException | ClassNotFoundException ex) {
            found.clear();
        }
        return found;
    }
}
