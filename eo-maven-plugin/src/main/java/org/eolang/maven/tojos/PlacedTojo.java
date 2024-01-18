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
package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;

/**
 * Placed tojo.
 *
 * @since 0.30
 */
public final class PlacedTojo {

    /**
     * The delegate.
     */
    private final Tojo origin;

    /**
     * Ctor.
     * @param tojo The delegate.
     */
    PlacedTojo(final Tojo tojo) {
        this.origin = tojo;
    }

    /**
     * The tojo id.
     * @return The id.
     */
    public String identifier() {
        return this.origin.get(PlacedTojos.Attribute.ID.key());
    }

    /**
     * The placed tojo dependency.
     * @return The dependency.
     */
    public String dependency() {
        return this.origin.get(PlacedTojos.Attribute.DEPENDENCY.key());
    }

    /**
     * The placed tojo related file path.
     * @return The related file path.
     */
    public String related() {
        return this.origin.get(PlacedTojos.Attribute.RELATED.key());
    }

    /**
     * Check if the tojo has the same hash.
     * @param hash The hash to check.
     * @return True if the hash is the same.
     */
    public boolean sameHash(final String hash) {
        return this.origin.get(PlacedTojos.Attribute.HASH.key()).equals(hash);
    }

    /**
     * Mark the tojo as unplaced.
     */
    public void unplace() {
        this.origin.set(PlacedTojos.Attribute.UNPLACED.key(), "true");
    }

    /**
     * Check if the tojo is a class.
     * @return True if the tojo is a class.
     */
    public boolean isClass() {
        return "class".equals(this.origin.get(PlacedTojos.Attribute.KIND.key()));
    }

    /**
     * Check if the tojo is a jar.
     * @return True if the tojo is a jar.
     */
    public boolean isJar() {
        return "jar".equals(this.origin.get(PlacedTojos.Attribute.KIND.key()));
    }

    /**
     * Check if the tojo is placed.
     * @return True if the tojo is placed.
     */
    public boolean placed() {
        return !this.unplaced();
    }

    /**
     * Check if the tojo is unplaced.
     * @return True if the tojo is unplaced.
     */
    public boolean unplaced() {
        return this.origin.exists(PlacedTojos.Attribute.UNPLACED.key())
            && "true".equals(this.origin.get(PlacedTojos.Attribute.UNPLACED.key()));
    }
}
