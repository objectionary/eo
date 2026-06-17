/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * EO object names discovered from classpath entries.
 * @since 1.0
 */
final class PhClasspathObjects {

    /**
     * Ctor.
     */
    private PhClasspathObjects() {
    }

    /**
     * Discover objects from classpath.
     * @return EO object names
     */
    static Collection<String> discover() {
        final Collection<String> objects = new TreeSet<>();
        for (final String item : PhClasspathObjects.classpath()) {
            objects.addAll(PhClasspathObjects.objects(item));
        }
        return objects;
    }

    /**
     * Objects by one classpath entry.
     * @param item Classpath item
     * @return EO object names
     */
    private static Collection<String> objects(final String item) {
        Collection<String> objects;
        try {
            final Path path = Paths.get(item);
            if (Files.isDirectory(path)) {
                objects = PhClasspathObjects.scan(path);
            } else if (Files.isRegularFile(path) && item.endsWith(".jar")) {
                objects = PhClasspathObjects.scan(path.toFile());
            } else {
                objects = Collections.emptyList();
            }
        } catch (final InvalidPathException | SecurityException ignored) {
            objects = Collections.emptyList();
        }
        return objects;
    }

    /**
     * Classpath entries.
     * @return Entries
     */
    private static Iterable<String> classpath() {
        return Arrays.stream(
            System.getProperty("java.class.path", "")
                .split(Pattern.quote(File.pathSeparator))
        ).filter(Predicate.not(String::isEmpty)).collect(Collectors.toList());
    }

    /**
     * Scan classpath directory.
     * @param root Classpath root
     * @return EO object names
     */
    private static Collection<String> scan(final Path root) {
        final Collection<String> found = new TreeSet<>();
        Collection<String> objects;
        final Path base = root.resolve("org").resolve("eolang");
        if (Files.exists(base)) {
            try (Stream<Path> paths = Files.walk(base)) {
                paths.filter(Files::isRegularFile)
                    .map(path -> root.relativize(path).toString())
                    .map(path -> path.replace(File.separatorChar, '/'))
                    .forEach(resource -> PhClasspathObjects.add(resource, found));
                objects = found;
            } catch (final IOException | SecurityException ignored) {
                objects = Collections.emptyList();
            }
        } else {
            objects = Collections.emptyList();
        }
        return objects;
    }

    /**
     * Scan JAR file.
     * @param jar JAR file
     * @return EO object names
     */
    private static Collection<String> scan(final File jar) {
        Collection<String> objects = new TreeSet<>();
        try (JarFile file = new JarFile(jar)) {
            final Enumeration<JarEntry> entries = file.entries();
            while (entries.hasMoreElements()) {
                final JarEntry entry = entries.nextElement();
                if (!entry.isDirectory()) {
                    PhClasspathObjects.add(entry.getName(), objects);
                }
            }
        } catch (final IOException | SecurityException ignored) {
            objects = Collections.emptyList();
        }
        return objects;
    }

    /**
     * Add object by class resource path.
     * @param resource Resource path
     * @param objects Objects
     */
    private static void add(final String resource, final Collection<String> objects) {
        objects.addAll(PhObjectNames.names(resource));
    }
}
