/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.io.ResourceOf;

/**
 * Short hex fingerprint of a set of classpath resources.
 *
 * <p>It reads each resource, in the given order, into one SHA-256
 * digest and returns the first twelve hex characters. It is used to
 * fold the content of the bundled transpile XSLs into the transpile
 * cache key, so that a change in the transformation logic invalidates
 * the cache even when the plugin version does not change (see #5578).</p>
 *
 * <p>A directory of files is hashed the same way, its files taken in the
 * order of their names, which is how the tables of {@link MjInference}
 * join the same key (see #7627). A directory that is not there hashes to
 * the digest of nothing, which is what a build without those tables
 * deserves and still tells it apart from a build with them.</p>
 *
 * <p>Callers name their resources the way
 * {@link Class#getResourceAsStream(String)} wants them, with a leading
 * slash, because the very same names also build XSL trains. The reading
 * here goes through the {@link ClassLoader} instead, which takes only
 * the global name, so the slash is dropped first.</p>
 *
 * @since 0.63
 */
final class Fingerprint implements Supplier<String> {

    /**
     * Classpath resource paths to hash, in order.
     */
    private final String[] resources;

    /**
     * The directories whose files to hash, in the order of their names.
     * Empty when only the resources are hashed.
     */
    private final Iterable<Path> dirs;

    /**
     * Ctor.
     * @param res Classpath resource paths to hash
     */
    Fingerprint(final String... res) {
        this(Collections.emptyList(), res);
    }

    /**
     * Ctor.
     * @param files The directory whose files to hash
     */
    Fingerprint(final Path files) {
        this(Collections.singletonList(files), new String[0]);
    }

    /**
     * Ctor.
     * @param files The directories whose files to hash
     * @param res Classpath resource paths to hash
     */
    private Fingerprint(final Iterable<Path> files, final String... res) {
        this.dirs = files;
        this.resources = res.clone();
    }

    @Override
    public String get() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (final String resource : this.resources) {
                digest.update(
                    new UncheckedBytes(
                        new BytesOf(
                            new ResourceOf(Fingerprint.global(resource), Fingerprint.class)
                        )
                    ).asBytes()
                );
            }
            for (final Path base : this.dirs) {
                if (Files.isDirectory(base)) {
                    try (Stream<Path> found = Files.walk(base)) {
                        for (final Path file : found.filter(Files::isRegularFile)
                            .sorted().collect(Collectors.toList())) {
                            digest.update(
                                base.relativize(file).toString().getBytes(StandardCharsets.UTF_8)
                            );
                            digest.update(Files.readAllBytes(file));
                        }
                    }
                }
            }
            final StringBuilder hex = new StringBuilder(64);
            for (final byte octet : digest.digest()) {
                hex.append(String.format("%02x", octet));
            }
            return hex.substring(0, 12);
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        } catch (final IOException ex) {
            throw new UncheckedIOException("Failed to read a resource while fingerprinting", ex);
        }
    }

    private static String global(final String resource) {
        final String global;
        if (resource.startsWith("/")) {
            global = resource.substring(1);
        } else {
            global = resource;
        }
        return global;
    }
}
