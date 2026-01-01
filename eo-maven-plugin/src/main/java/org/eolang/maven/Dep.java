/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.function.Supplier;
import javax.annotation.Nonnull;
import org.apache.maven.model.Dependency;

/**
 * Wrapped for maven model {@link Dependency}.
 *
 * @since 0.54
 */
final class Dep implements Comparable<Dep>, Supplier<Dependency> {

    /**
     * The dependency.
     */
    private final Dependency dependency;

    /**
     * Ctor.
     */
    Dep() {
        this(new Dependency());
    }

    /**
     * Ctor.
     * @param dep The dependency
     */
    Dep(final Dependency dep) {
        this.dependency = dep;
    }

    @Override
    public String toString() {
        final String ret;
        if (this.dependency.getClassifier() == null || this.dependency.getClassifier().isEmpty()) {
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
    public int compareTo(@Nonnull final Dep other) {
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

    @Override
    public Dependency get() {
        return this.dependency;
    }

    /**
     * Set group id.
     * @param group Group id
     * @return Self
     */
    Dep withGroupId(final String group) {
        this.dependency.setGroupId(group);
        return this;
    }

    /**
     * Set artifact id.
     * @param artifact Artifact id
     * @return Self
     */
    Dep withArtifactId(final String artifact) {
        this.dependency.setArtifactId(artifact);
        return this;
    }

    /**
     * Set version.
     * @param version Version
     * @return Self
     */
    Dep withVersion(final String version) {
        this.dependency.setVersion(version);
        return this;
    }

    /**
     * Set scope.
     * @param scope Scope
     * @return Self
     */
    Dep withScope(final String scope) {
        this.dependency.setScope(scope);
        return this;
    }

    /**
     * Set classifier id.
     * @param classifier Classifier
     * @return Self
     */
    Dep withClassifier(final String classifier) {
        this.dependency.setClassifier(classifier);
        return this;
    }
}
