/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * Invalidates the local EO cache when the plugin JAR changes.
 * Only applies to SNAPSHOT versions — released versions use version-keyed
 * cache directories that never collide.
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
     * Wipes all SNAPSHOT stage caches if the plugin JAR has changed.
     */
    void exec() {
        if (!this.version.contains("SNAPSHOT")) {
            return;
        }
        try {
            final String current = new Sha(
                MjSafe.class.getProtectionDomain().getCodeSource().getLocation().openStream()
            ).toString();
            final Path fingerprint = this.cache.resolve(Janitor.FINGERPRINT);
            if (Files.exists(fingerprint)
                && new String(Files.readAllBytes(fingerprint), StandardCharsets.UTF_8)
                    .equals(current)) {
                return;
            }
            Logger.info(
                this,
                "Plugin JAR changed, invalidating SNAPSHOT cache at %[file]s",
                this.cache
            );
            for (final String stage : Janitor.STAGES) {
                final Path dir = this.cache.resolve(stage).resolve(this.version);
                if (Files.exists(dir)) {
                    Janitor.wipe(dir);
                }
            }
            new Saved(current, fingerprint).value();
        } catch (final IOException ex) {
            throw new IllegalStateException("Failed to clean SNAPSHOT cache", ex);
        }
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
