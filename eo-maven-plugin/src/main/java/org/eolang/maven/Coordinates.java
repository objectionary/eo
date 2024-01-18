/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import javax.annotation.Nonnull;
import org.apache.maven.model.Dependency;

/**
 * Maven coordinates as a string.
 *
 * @since 0.29.0
 */
public final class Coordinates implements Comparable<Coordinates> {

    /**
     * The dependency.
     */
    private final Dependency dependency;

    /**
     * Ctor.
     * @param dep The dependency
     */
    public Coordinates(final Dependency dep) {
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
