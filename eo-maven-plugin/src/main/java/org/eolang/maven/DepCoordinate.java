/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.model.Dependency;

/**
 * A dependency's coordinate, without its version: {@code groupId:artifactId:classifier}.
 *
 * @since 0.61.0
 */
final class DepCoordinate {

    /**
     * The dependency.
     */
    private final Dependency dependency;

    /**
     * Ctor.
     *
     * @param dep The dependency
     */
    DepCoordinate(final Dependency dep) {
        this.dependency = dep;
    }

    /**
     * The coordinate string.
     *
     * @return The coordinate, without the version
     */
    String value() {
        return String.join(
            ":",
            this.dependency.getGroupId(),
            this.dependency.getArtifactId(),
            this.classifier()
        );
    }

    /**
     * The dependency's classifier, normalized to {@code "-"} when absent.
     *
     * @return The classifier
     */
    String classifier() {
        final String classifier;
        if (this.dependency.getClassifier() == null
            || this.dependency.getClassifier().isEmpty()) {
            classifier = "-";
        } else {
            classifier = this.dependency.getClassifier();
        }
        return classifier;
    }
}
