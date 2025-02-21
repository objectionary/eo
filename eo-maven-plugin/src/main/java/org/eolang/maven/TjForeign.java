/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.jcabi.log.Logger;
import com.yegor256.tojos.Tojo;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * Foreign tojo.
 *
 * @since 0.30
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
final class TjForeign {

    /**
     * The delegate.
     */
    private final Tojo delegate;

    /**
     * Ctor.
     * @param original The delegate.
     */
    TjForeign(final Tojo original) {
        this.delegate = original;
    }

    @Override
    public String toString() {
        return this.delegate.toString();
    }

    @Override
    public boolean equals(final Object other) {
        final boolean result;
        if (this == other) {
            result = true;
        } else if (other == null || this.getClass() != other.getClass()) {
            result = false;
        } else {
            final TjForeign tojo = (TjForeign) other;
            result = Objects.equals(this.delegate, tojo.delegate);
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.delegate);
    }

    /**
     * The id of the tojo.
     * @return The id of the tojo.
     */
    String identifier() {
        return this.attribute(TjsForeign.Attribute.ID);
    }

    /**
     * The tojo xmir.
     * @return The xmir.
     */
    Path xmir() {
        return Paths.get(this.attribute(TjsForeign.Attribute.XMIR));
    }

    /**
     * The tojo shaken xmir.
     * @return The shaken xmir.
     */
    Path shaken() {
        return Paths.get(this.attribute(TjsForeign.Attribute.SHAKEN));
    }

    /**
     * The tojo linted xmir.
     * @return The shaken xmir.
     */
    Path linted() {
        return Paths.get(this.attribute(TjsForeign.Attribute.LINTED));
    }

    /**
     * The tojo eo object.
     * @return The eo object.
     */
    Path source() {
        return Paths.get(this.attribute(TjsForeign.Attribute.EO));
    }

    /**
     * The tojo version.
     * @return The version.
     */
    String version() {
        return this.attribute(TjsForeign.Attribute.VERSION);
    }

    /**
     * The tojo description.
     * @return The description.
     */
    String description() {
        return String.format(
            "%s:%s",
            this.attribute(TjsForeign.Attribute.ID),
            this.version()
        );
    }

    /**
     * The tojo hash.
     * @return The hash.
     */
    String hash() {
        return this.attribute(TjsForeign.Attribute.HASH);
    }

    /**
     * The tojo probed.
     * @return The probed.
     */
    String probed() {
        return this.attribute(TjsForeign.Attribute.PROBED);
    }

    /**
     * The discovered at location.
     * @return The discovered at.
     */
    String discoveredAt() {
        return this.attribute(TjsForeign.Attribute.DISCOVERED_AT);
    }

    /**
     * Checks if tojo was not already shaken.
     * @return True if shake is required, false otherwise.
     */
    boolean notShaken() {
        final Path src = this.xmir();
        boolean res = true;
        if (this.delegate.exists(TjsForeign.Attribute.SHAKEN.getKey())) {
            final Path tgt = this.shaken();
            if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(this, "Already shaken %[file]s to %[file]s", src, tgt);
                res = false;
            }
        }
        return res;
    }

    /**
     * Check if the given tojo has not been parsed.
     *
     * @return True if the tojo has not been parsed.
     */
    boolean notParsed() {
        boolean res = true;
        if (this.delegate.exists(TjsForeign.Attribute.XMIR.getKey())) {
            final Path xmir = this.xmir();
            if (xmir.toFile().lastModified() >= this.source().toFile().lastModified()) {
                Logger.debug(
                    this, "Already parsed %s to %[file]s (it's newer than the source)",
                    this.identifier(), xmir
                );
                res = false;
            }
        }
        return res;
    }

    /**
     * Checks if tojo has hash.
     * @return True if has hash, false otherwise.
     */
    boolean hasHash() {
        return this.delegate.exists(TjsForeign.Attribute.HASH.getKey());
    }

    /**
     * Set the jar.
     * @param coordinates The coordinates of jar.
     * @return The tojo itself.
     */
    TjForeign withJar(final Coordinates coordinates) {
        this.delegate.set(TjsForeign.Attribute.JAR.getKey(), coordinates.toString());
        return this;
    }

    /**
     * Set the discovered at.
     * @param path The path where was discovered.
     * @return The tojo itself.
     */
    TjForeign withDiscoveredAt(final Path path) {
        if (!this.delegate.exists(TjsForeign.Attribute.VERSION.getKey())) {
            this.delegate.set(TjsForeign.Attribute.VERSION.getKey(), "*.*.*");
        }
        this.delegate.set(TjsForeign.Attribute.DISCOVERED_AT.getKey(), path);
        return this;
    }

    /**
     * Set sodg.
     * @param sodg Sodg.
     * @return The tojo itself.
     */
    TjForeign withSodg(final Path sodg) {
        this.delegate.set(TjsForeign.Attribute.SODG.getKey(), sodg.toString());
        return this;
    }

    /**
     * Set the shaken xmir.
     * @param xmir The shaken xmir.
     * @return The tojo itself.
     */
    TjForeign withShaken(final Path xmir) {
        this.delegate.set(TjsForeign.Attribute.SHAKEN.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the linted xmir.
     * @param xmir The linted xmir.
     * @return The tojo itself.
     */
    TjForeign withLinted(final Path xmir) {
        this.delegate.set(TjsForeign.Attribute.LINTED.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the eo path.
     * @param source The eo path.
     * @return The tojo itself.
     */
    TjForeign withSource(final Path source) {
        this.delegate.set(TjsForeign.Attribute.EO.getKey(), source.toString());
        return this;
    }

    /**
     * Set the hash.
     * @param hash The hash.
     * @return The tojo itself.
     */
    TjForeign withHash(final CommitHash hash) {
        this.delegate.set(TjsForeign.Attribute.HASH.getKey(), hash.value());
        return this;
    }

    /**
     * Set the number of probed objects.
     * @param count The number of probed objects.
     * @return The tojo itself.
     */
    TjForeign withProbed(final int count) {
        this.delegate.set(TjsForeign.Attribute.PROBED.getKey(), Integer.toString(count));
        return this;
    }

    /**
     * Set the xmir.
     * @param xmir The xmir.
     * @return The tojo itself.
     */
    TjForeign withXmir(final Path xmir) {
        this.delegate.set(TjsForeign.Attribute.XMIR.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the version.
     * @param ver The version.
     * @return The tojo itself.
     */
    TjForeign withVersion(final String ver) {
        this.delegate.set(TjsForeign.Attribute.VERSION.getKey(), ver);
        return this;
    }

    /**
     * Set the scope.
     * @param scope The scope.
     * @return The tojo itself.
     */
    TjForeign withScope(final String scope) {
        this.delegate.set(TjsForeign.Attribute.SCOPE.getKey(), scope);
        return this;
    }

    /**
     * Return the scope of the tojo.
     * @return The scope.
     */
    String scope() {
        return this.attribute(TjsForeign.Attribute.SCOPE);
    }

    /**
     * Return the attribute from the tojo.
     * @param attribute The attribute from ForeignTojos.Attribute.
     * @return The attribute.
     */
    private String attribute(final TjsForeign.Attribute attribute) {
        final String attr = this.delegate.get(attribute.getKey());
        if (attr == null) {
            throw new AttributeNotFoundException(attribute);
        }
        return attr;
    }
}
