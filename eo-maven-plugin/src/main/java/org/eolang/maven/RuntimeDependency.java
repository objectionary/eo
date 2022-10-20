/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import org.apache.maven.model.Dependency;

/**
 * RuntimeDependency is a class for keeping and getting the last eo-runtime dependency.
 *
 * @since 0.28.11
 */
final class RuntimeDependency {
    /**
     * EO runtime dependency group.
     */
    private final String group;

    /**
     * EO runtime dependency artifact name.
     */
    private final String artifact;

    /**
     * EO runtime dependency version.
     */
    private final String version;

    /**
     * Default constructor.
     */
    RuntimeDependency() {
        this("0.28.10");
    }

    /**
     * Constructor with version.
     *
     * @param version Version of the eo-runtime library
     */
    RuntimeDependency(final String version) {
        this("org.eolang", "eo-runtime", version);
    }

    /**
     * The main constructor.
     *
     * @param group Dependency group
     * @param artifact Dependency artifact name
     * @param version Dependency version
     */
    RuntimeDependency(final String group, final String artifact, final String version) {
        this.group = group;
        this.artifact = artifact;
        this.version = version;
    }

    /**
     * Compares current dependency with other dependency.
     *
     * @param other Other dependency
     * @return True if other dependency is the eo-runtime dependency
     */
    boolean theSameAs(final Dependency other) {
        return this.group.equals(other.getGroupId())
            && this.artifact.equals(other.getArtifactId());
    }

    /**
     * Converts RuntimeDependency to Maven Dependency.
     *
     * @return Maven Dependency
     */
    Dependency toDependency() {
        final Dependency dependency = new Dependency();
        dependency.setGroupId(this.group);
        dependency.setArtifactId(this.artifact);
        dependency.setVersion(this.version);
        dependency.setClassifier("");
        return dependency;
    }
}
