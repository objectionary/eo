/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.function.Supplier;

/**
 * Short hex fingerprint of a set of classpath resources.
 *
 * <p>It reads each resource, in the given order, into one SHA-256
 * digest and returns the first twelve hex characters. It is used to
 * fold the content of the bundled transpile XSLs into the transpile
 * cache key, so that a change in the transformation logic invalidates
 * the cache even when the plugin version does not change (see #5578).</p>
 *
 * @since 0.63
 */
final class Fingerprint implements Supplier<String> {

    /**
     * Classpath resource paths to hash, in order.
     */
    private final String[] resources;

    /**
     * Ctor.
     * @param res Classpath resource paths to hash
     */
    Fingerprint(final String... res) {
        this.resources = res.clone();
    }

    @Override
    public String get() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (final String resource : this.resources) {
                try (InputStream input = Fingerprint.class.getResourceAsStream(resource)) {
                    if (input == null) {
                        throw new IllegalStateException(
                            String.format("Resource '%s' not found on classpath", resource)
                        );
                    }
                    digest.update(input.readAllBytes());
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
}
