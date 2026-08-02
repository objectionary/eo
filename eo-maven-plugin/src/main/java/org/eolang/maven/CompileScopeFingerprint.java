/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.Comparator;
import java.util.function.Supplier;

/**
 * Fingerprint of a set of compile-scope tojos, keyed on each one's
 * identifier and the content of its XMIR.
 *
 * <p>Used to fold the compile-scope XMIRs a whole-program-analysis pass
 * reads into that pass's cache key, so a change confined to a compile-scope
 * dependency's XMIR invalidates the cached verdict the same way a change in
 * the project's own sources already does (see {@link Linting}, #6214).</p>
 *
 * @since 0.64
 */
final class CompileScopeFingerprint implements Supplier<String> {

    /**
     * The compile-scope tojos to fingerprint.
     */
    private final Collection<TjForeign> tojos;

    /**
     * Ctor.
     * @param compile Compile-scope tojos
     */
    CompileScopeFingerprint(final Collection<TjForeign> compile) {
        this.tojos = compile;
    }

    @Override
    public String get() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            this.tojos.stream()
                .sorted(Comparator.comparing(TjForeign::identifier)).forEach(
                    tojo -> digest.update(
                        String.format("%s:%s%n", tojo.identifier(), new Sha(tojo.xmir()))
                            .getBytes(StandardCharsets.UTF_8)
                    )
                );
            final StringBuilder hex = new StringBuilder(64);
            for (final byte octet : digest.digest()) {
                hex.append(String.format("%02x", octet));
            }
            return hex.toString();
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        }
    }
}
