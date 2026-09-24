/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.function.Supplier;

/**
 * Short hex digest of a string.
 *
 * <p>Used to fold an unbounded piece of text (e.g. a set of names) into a
 * fixed-width cache-key segment, so a filesystem path built from it never
 * risks running past the 255-byte component limit ext4 and APFS enforce.</p>
 *
 * @since 0.64
 */
final class Hashed implements Supplier<String> {

    /**
     * The text to hash.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param txt The text to hash
     */
    Hashed(final String txt) {
        this.text = txt;
    }

    @Override
    public String get() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            digest.update(this.text.getBytes(StandardCharsets.UTF_8));
            final StringBuilder hex = new StringBuilder(64);
            for (final byte octet : digest.digest()) {
                hex.append(String.format("%02x", octet));
            }
            return hex.substring(0, 12);
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        }
    }
}
