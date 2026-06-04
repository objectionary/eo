/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * Invalidates the local EO cache on every SNAPSHOT build.
 * Released versions use version-keyed cache directories that never collide,
 * so no invalidation is needed for them.
 * @see <a href="https://github.com/objectionary/eo/issues/5179">issue #5179</a>
 * @since 0.62.0
 */
final class Janitor {

    /**
     * Fingerprint file stored inside the cache root.
     */
    private static final String FINGERPRINT = ".snapshot-fingerprint";

    /**
     * Stage directories that share the versioned cache layout.
     */
    private static final String[] STAGES = {"parsed", "transpiled", "linted", "pulled"};

    /**
     * Base cache directory, e.g. ~/.eo/.
     */
    private final Path cache;

    /**
     * Current plugin version, e.g. "1.0-SNAPSHOT".
     */
    private final String version;

    /**
     * Ctor.
     * @param cache Base cache directory
     * @param version Plugin version
     */
    Janitor(final Path cache, final String version) {
        this.cache = cache;
        this.version = version;
    }

    /**
     * Wipes all SNAPSHOT stage caches on every build.
     * Does nothing for released versions.
     */
    void clean() {
        if (this.version.contains("SNAPSHOT")) {
            try {
                this.invalidate();
            } catch (final IOException | URISyntaxException ex) {
                throw new IllegalStateException("Failed to clean SNAPSHOT cache", ex);
            }
        }
    }

    /**
     * Checks the fingerprint and wipes caches if it has changed.
     * @throws IOException If an IO operation fails
     * @throws URISyntaxException If the code source location URI is malformed
     */
    private void invalidate() throws IOException, URISyntaxException {
        final String current = String.valueOf(Janitor.mtime());
        final Path fingerprint = this.cache.resolve(Janitor.FINGERPRINT);
        if (!Files.exists(fingerprint)
            || !Files.readString(fingerprint).equals(current)) {
            this.wipeCaches();
            new Saved(current, fingerprint).value();
        }
    }

    /**
     * Deletes the versioned cache directory for each stage if it exists.
     * @throws IOException If deletion fails
     */
    private void wipeCaches() throws IOException {
        for (final String stage : Janitor.STAGES) {
            final Path dir = this.cache.resolve(stage).resolve(this.version);
            if (Files.exists(dir)) {
                Logger.info(
                    this,
                    "Plugin code changed, invalidating SNAPSHOT cache at %[file]s",
                    dir
                );
                Janitor.wipe(dir);
            }
        }
    }

    /**
     * Returns the modification time of the plugin JAR or classes directory.
     * Used as a build fingerprint: changes on every build, so comparing it
     * to the stored value always detects a new build.
     * @return Last-modified timestamp in milliseconds
     * @throws URISyntaxException If the code source location URI is malformed
     * @throws IOException If reading the directory fails
     */
    private static long mtime() throws URISyntaxException, IOException {
        final Path location = Paths.get(
            Janitor.class.getProtectionDomain().getCodeSource().getLocation().toURI()
        );
        final long result;
        if (Files.isDirectory(location)) {
            try (Stream<Path> stream = Files.walk(location)) {
                result = stream.filter(Files::isRegularFile)
                    .mapToLong(f -> f.toFile().lastModified())
                    .max()
                    .orElse(0L);
            }
        } else {
            result = location.toFile().lastModified();
        }
        return result;
    }

    /**
     * Recursively deletes a directory.
     * @param dir Directory to delete
     * @throws IOException If deletion fails
     */
    private static void wipe(final Path dir) throws IOException {
        try (Stream<Path> stream = Files.walk(dir)) {
            stream.sorted(Comparator.reverseOrder()).forEach(
                path -> {
                    try {
                        Files.delete(path);
                    } catch (final IOException ex) {
                        throw new IllegalStateException(
                            String.format("Failed to delete %s", path), ex
                        );
                    }
                }
            );
        }
    }
}
