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

import java.util.function.Predicate;
import org.apache.maven.model.Dependency;

/**
 * This is a predicate that checks Dependency group and artifactId
 * that should be equal to 'group' or 'artifact' of eo-runtime dependency.
 *
 * @since 0.28.11
 */
final class RuntimeDependencyEquality implements Predicate<Dependency> {

    /**
     * EO runtime dependency group.
     */
    private final String group;

    /**
     * EO runtime dependency artifact name.
     */
    private final String artifact;

    /**
     * Default constructor.
     */
    RuntimeDependencyEquality() {
        this("org.eolang", "eo-runtime");
    }

    /**
     * The main constructor.
     *
     * @param group Dependency group
     * @param artifact Dependency artifact name
     */
    private RuntimeDependencyEquality(final String group, final String artifact) {
        this.group = group;
        this.artifact = artifact;
    }

    @Override
    public boolean test(final Dependency other) {
        return this.group.equals(other.getGroupId())
            && this.artifact.equals(other.getArtifactId());
    }
}
