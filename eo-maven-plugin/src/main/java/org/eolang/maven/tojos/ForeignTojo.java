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

import com.jcabi.log.Logger;
import com.yegor256.tojos.Tojo;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import org.eolang.maven.Coordinates;
import org.eolang.maven.hash.CommitHash;

/**
 * Foreign tojo.
 *
 * @since 0.30
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
public final class ForeignTojo {

    /**
     * The delegate.
     */
    private final Tojo delegate;

    /**
     * Ctor.
     * @param original The delegate.
     */
    public ForeignTojo(final Tojo original) {
        this.delegate = original;
    }

    /**
     * The id of the tojo.
     * @return The id of the tojo.
     */
    public String identifier() {
        return this.attribute(ForeignTojos.Attribute.ID);
    }

    /**
     * The tojo xmir.
     * @return The xmir.
     */
    public Path xmir() {
        return Paths.get(this.attribute(ForeignTojos.Attribute.XMIR));
    }

    /**
     * The tojo optimized xmir.
     * @return The optimized xmir.
     */
    public Path optimized() {
        return Paths.get(this.attribute(ForeignTojos.Attribute.OPTIMIZED));
    }

    /**
     * The tojo verified xmir.
     * @return The verified xmir.
     */
    public Path verified() {
        return Paths.get(this.attribute(ForeignTojos.Attribute.VERIFIED));
    }

    /**
     * The tojo shaken xmir.
     * @return The shaken xmir.
     */
    public Path shaken() {
        return Paths.get(this.attribute(ForeignTojos.Attribute.SHAKEN));
    }

    /**
     * The tojo eo object.
     * @return The eo object.
     */
    public Path source() {
        return Paths.get(this.attribute(ForeignTojos.Attribute.EO));
    }

    /**
     * The tojo version.
     * @return The version.
     */
    public String version() {
        return this.attribute(ForeignTojos.Attribute.VERSION);
    }

    /**
     * The tojo description.
     * @return The description.
     */
    public String description() {
        return String.format(
            "%s:%s",
            this.attribute(ForeignTojos.Attribute.ID),
            this.version()
        );
    }

    /**
     * The tojo hash.
     * @return The hash.
     */
    public String hash() {
        return this.attribute(ForeignTojos.Attribute.HASH);
    }

    /**
     * The tojo probed.
     * @return The probed.
     */
    public String probed() {
        return this.attribute(ForeignTojos.Attribute.PROBED);
    }

    /**
     * The discovered at location.
     * @return The discovered at.
     */
    public String discoveredAt() {
        return this.attribute(ForeignTojos.Attribute.DISCOVERED_AT);
    }

    /**
     * Checks if tojo was not already optimized.
     *
     * @return True if optimization is required, false otherwise.
     */
    public boolean notOptimized() {
        final Path src = this.xmir();
        boolean res = true;
        if (this.delegate.exists(ForeignTojos.Attribute.OPTIMIZED.getKey())) {
            final Path tgt = this.optimized();
            if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(this, "Already optimized %[file]s to %[file]s", src, tgt);
                res = false;
            }
        }
        return res;
    }

    /**
     * Checks if tojo was not already shaken.
     * @return True if shake is required, false otherwise.
     */
    public boolean notShaken() {
        final Path src = this.optimized();
        boolean res = true;
        if (this.delegate.exists(ForeignTojos.Attribute.SHAKEN.getKey())) {
            final Path tgt = this.shaken();
            if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(this, "Already shaken %[file]s to %[file]s", src, tgt);
                res = false;
            }
        }
        return res;
    }

    /**
     * Checks if tojo was not already verified.
     *
     * @return True if optimization is required, false otherwise.
     */
    public boolean notVerified() {
        final Path src = this.xmir();
        boolean res = true;
        if (this.delegate.exists(ForeignTojos.Attribute.VERIFIED.getKey())) {
            final Path tgt = this.verified();
            if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(this, "Already verified %[file]s to %[file]s", src, tgt);
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
    public boolean notParsed() {
        boolean res = true;
        if (this.delegate.exists(ForeignTojos.Attribute.XMIR.getKey())) {
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
    public boolean hasHash() {
        return this.delegate.exists(ForeignTojos.Attribute.HASH.getKey());
    }

    /**
     * Set the jar.
     * @param coordinates The coordinates of jar.
     * @return The tojo itself.
     */
    public ForeignTojo withJar(final Coordinates coordinates) {
        this.delegate.set(ForeignTojos.Attribute.JAR.getKey(), coordinates.toString());
        return this;
    }

    /**
     * Set the discovered size.
     * @param size The size.
     * @return The tojo itself.
     */
    public ForeignTojo withDiscovered(final int size) {
        this.delegate.set(ForeignTojos.Attribute.DISCOVERED.getKey(), Integer.valueOf(size));
        return this;
    }

    /**
     * Set the discovered at.
     * @param path The path where was discovered.
     * @return The tojo itself.
     */
    public ForeignTojo withDiscoveredAt(final Path path) {
        if (!this.delegate.exists(ForeignTojos.Attribute.VERSION.getKey())) {
            this.delegate.set(ForeignTojos.Attribute.VERSION.getKey(), "*.*.*");
        }
        this.delegate.set(ForeignTojos.Attribute.DISCOVERED_AT.getKey(), path);
        return this;
    }

    /**
     * Set sodg.
     * @param sodg Sodg.
     * @return The tojo itself.
     */
    public ForeignTojo withSodg(final Path sodg) {
        this.delegate.set(ForeignTojos.Attribute.SODG.getKey(), sodg.toString());
        return this;
    }

    /**
     * Set the optimized xmir.
     * @param xmir The optimized xmir.
     * @return The tojo itself.
     */
    public ForeignTojo withOptimized(final Path xmir) {
        this.delegate.set(ForeignTojos.Attribute.OPTIMIZED.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the shaken xmir.
     * @param xmir The shaken xmir.
     * @return The tojo itself.
     */
    public ForeignTojo withShaken(final Path xmir) {
        this.delegate.set(ForeignTojos.Attribute.SHAKEN.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the verified xmir.
     * @param xmir The verified xmir.
     * @return The tojo itself.
     */
    public ForeignTojo withVerified(final Path xmir) {
        this.delegate.set(ForeignTojos.Attribute.VERIFIED.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the eo path.
     * @param source The eo path.
     * @return The tojo itself.
     */
    public ForeignTojo withSource(final Path source) {
        this.delegate.set(ForeignTojos.Attribute.EO.getKey(), source.toString());
        return this;
    }

    /**
     * Set the hash.
     * @param hash The hash.
     * @return The tojo itself.
     */
    public ForeignTojo withHash(final CommitHash hash) {
        this.delegate.set(ForeignTojos.Attribute.HASH.getKey(), hash.value());
        return this;
    }

    /**
     * Set the number of probed objects.
     * @param count The number of probed objects.
     * @return The tojo itself.
     */
    public ForeignTojo withProbed(final int count) {
        this.delegate.set(ForeignTojos.Attribute.PROBED.getKey(), Integer.toString(count));
        return this;
    }

    /**
     * Set the xmir.
     * @param xmir The xmir.
     * @return The tojo itself.
     */
    public ForeignTojo withXmir(final Path xmir) {
        this.delegate.set(ForeignTojos.Attribute.XMIR.getKey(), xmir.toString());
        return this;
    }

    /**
     * Set the version.
     * @param ver The version.
     * @return The tojo itself.
     */
    public ForeignTojo withVersion(final String ver) {
        this.delegate.set(ForeignTojos.Attribute.VERSION.getKey(), ver);
        return this;
    }

    /**
     * Set the scope.
     * @param scope The scope.
     * @return The tojo itself.
     */
    public ForeignTojo withScope(final String scope) {
        this.delegate.set(ForeignTojos.Attribute.SCOPE.getKey(), scope);
        return this;
    }

    /**
     * Return the scope of the tojo.
     * @return The scope.
     */
    public String scope() {
        return this.attribute(ForeignTojos.Attribute.SCOPE);
    }

    /**
     * Temporary version.
     * @param ver The version.
     * @return The tojo itself.
     */
    public ForeignTojo withVer(final String ver) {
        this.delegate.set(ForeignTojos.Attribute.VER.getKey(), ver);
        return this;
    }

    /**
     * Return the temporary version of the tojo.
     * @return The version.
     */
    public String ver() {
        return this.attribute(ForeignTojos.Attribute.VER);
    }

    @Override
    public boolean equals(final Object other) {
        final boolean result;
        if (this == other) {
            result = true;
        } else if (other == null || this.getClass() != other.getClass()) {
            result = false;
        } else {
            final ForeignTojo tojo = (ForeignTojo) other;
            result = Objects.equals(this.delegate, tojo.delegate);
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.delegate);
    }

    /**
     * Return the attribute from the tojo.
     * @param attribute The attribute from ForeignTojos.Attribute.
     * @return The attribute.
     */
    private String attribute(final ForeignTojos.Attribute attribute) {
        final String attr = this.delegate.get(attribute.getKey());
        if (attr == null) {
            throw new AttributeNotFoundException(attribute);
        }
        return attr;
    }
}
