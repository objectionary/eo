/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import javax.annotation.Nonnull;
import org.apache.maven.model.Dependency;

/**
 * Maven coordinates as a string.
 *
 * @since 0.29.0
 */
final class Coordinates implements Comparable<Coordinates> {

    /**
     * The dependency.
     */
    private final Dependency dependency;

    /**
     * Ctor.
     * @param dep The dependency
     */
    Coordinates(final Dependency dep) {
        this.dependency = dep;
    }

    @Override
    public String toString() {
        final String ret;
        if (this.dependency.getClassifier() == null
            || this.dependency.getClassifier().isEmpty()) {
            ret = String.format(
                "%s:%s:%s",
                this.dependency.getGroupId(),
                this.dependency.getArtifactId(),
                this.dependency.getVersion()
            );
        } else {
            ret = String.format(
                "%s:%s:%s:%s",
                this.dependency.getGroupId(),
                this.dependency.getArtifactId(),
                this.dependency.getClassifier(),
                this.dependency.getVersion()
            );
        }
        return ret;
    }

    @Override
    public int compareTo(@Nonnull final Coordinates other) {
        return this.toString().compareTo(String.valueOf(other));
    }

    @Override
    public boolean equals(@Nonnull final Object other) {
        return this.toString().equals(String.valueOf(other));
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
}
