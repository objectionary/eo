/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Supplier;

/**
 * Fingerprint of XMIR inputs, skip flags and the WPA artifact version that
 * change a WPA verdict.
 *
 * @since 0.62.0
 */
final class WpaCacheKey implements Supplier<String> {

    /**
     * XMIR files WPA reads, keyed by identifier.
     */
    private final Map<String, Path> paths;

    /**
     * Program lints to skip.
     */
    private final Collection<String> skips;

    /**
     * Whether experimental lints are skipped.
     */
    private final boolean experimental;

    /**
     * The version of the {@code org.eolang:wpa} artifact deciding the
     * verdict, so an artifact upgrade with everything else unchanged still
     * invalidates the cache.
     */
    private final String version;

    /**
     * Ctor.
     *
     * @param files XMIR files WPA reads
     * @param programlints Program lints to skip
     * @param skip Whether experimental lints are skipped
     * @param wpa The version of the {@code org.eolang:wpa} artifact in use
     */
    WpaCacheKey(final Map<String, Path> files, final Collection<String> programlints,
        final boolean skip, final String wpa) {
        this.paths = files;
        this.skips = programlints;
        this.experimental = skip;
        this.version = wpa;
    }

    @Override
    public String get() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            for (final Map.Entry<String, Path> ent : new TreeMap<>(this.paths).entrySet()) {
                digest.update(ent.getKey().getBytes(StandardCharsets.UTF_8));
                digest.update((byte) 0);
                digest.update(new Sha(ent.getValue()).toString().getBytes(StandardCharsets.UTF_8));
                digest.update((byte) 0);
            }
            final List<String> sorted = new ArrayList<>(this.skips);
            sorted.sort(String::compareTo);
            for (final String lint : sorted) {
                digest.update(lint.getBytes(StandardCharsets.UTF_8));
                digest.update((byte) 0);
            }
            if (this.experimental) {
                digest.update((byte) 1);
            } else {
                digest.update((byte) 0);
            }
            digest.update(this.version.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest.digest());
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available for WPA cache key", ex);
        }
    }
}
