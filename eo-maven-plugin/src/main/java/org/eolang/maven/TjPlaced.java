/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.Tojo;

/**
 * Placed tojo.
 *
 * @since 0.30
 */
final class TjPlaced {

    /**
     * The delegate.
     */
    private final Tojo origin;

    /**
     * Ctor.
     * @param tojo The delegate.
     */
    TjPlaced(final Tojo tojo) {
        this.origin = tojo;
    }

    /**
     * The tojo id.
     * @return The id.
     */
    String identifier() {
        return this.origin.get(TjsPlaced.Attribute.ID.getKey());
    }

    /**
     * The placed tojo dependency.
     * @return The dependency.
     */
    String dependency() {
        return this.origin.get(TjsPlaced.Attribute.DEPENDENCY.getKey());
    }

    /**
     * The placed tojo related file path.
     * @return The related file path.
     */
    String related() {
        return this.origin.get(TjsPlaced.Attribute.RELATED.getKey());
    }

    /**
     * Check if the tojo has the same hash.
     * @param hash The hash to check.
     * @return True if the hash is the same.
     */
    boolean sameHash(final String hash) {
        return this.origin.get(TjsPlaced.Attribute.HASH.getKey()).equals(hash);
    }

    /**
     * Mark the tojo as unplaced.
     */
    void unplace() {
        this.origin.set(TjsPlaced.Attribute.UNPLACED.getKey(), "true");
    }

    /**
     * Check if the tojo is a class.
     * @return True if the tojo is a class.
     */
    boolean isClass() {
        return "class".equals(this.origin.get(TjsPlaced.Attribute.KIND.getKey()));
    }

    /**
     * Check if the tojo is a jar.
     * @return True if the tojo is a jar.
     */
    boolean isJar() {
        return "jar".equals(this.origin.get(TjsPlaced.Attribute.KIND.getKey()));
    }

    /**
     * Check if the tojo is placed.
     * @return True if the tojo is placed.
     */
    boolean placed() {
        return !this.unplaced();
    }

    /**
     * Check if the tojo is unplaced.
     * @return True if the tojo is unplaced.
     */
    boolean unplaced() {
        return this.origin.exists(TjsPlaced.Attribute.UNPLACED.getKey())
            && "true".equals(this.origin.get(TjsPlaced.Attribute.UNPLACED.getKey()));
    }
}
