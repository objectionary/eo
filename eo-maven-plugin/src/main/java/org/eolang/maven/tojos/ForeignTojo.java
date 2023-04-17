/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Coordinates;
import org.eolang.maven.util.Rel;

/**
 * Foreign tojo.
 *
 * @since 0.30
 */
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
    public String id() {
        return this.delegate.get(ForeignTojos.Attribute.ID.key());
    }

    /**
     * The tojo xmir.
     * @return The xmir.
     */
    public Path xmir() {
        return Paths.get(this.delegate.get(ForeignTojos.Attribute.XMIR.key()));
    }

    /**
     * The tojo xmir2.
     * @return The xmir2.
     */
    public Path xmirSecond() {
        return Paths.get(this.delegate.get(ForeignTojos.Attribute.XMIR_2.key()));
    }

    /**
     * The tojo eo object.
     * @return The eo object.
     */
    public Path eolangObject() {
        return Paths.get(this.delegate.get(ForeignTojos.Attribute.EO.key()));
    }

    /**
     * The tojo version.
     * @return The version.
     */
    public String version() {
        return this.delegate.get(ForeignTojos.Attribute.VERSION.key());
    }

    /**
     * The tojo description.
     * @return The description.
     */
    public String shortDescription() {
        return String.format(
            "%s:%s",
            this.delegate.get(ForeignTojos.Attribute.ID.key()),
            this.version()
        );
    }

    /**
     * The tojo hash.
     * @return The hash.
     */
    public String hash(){
        return this.delegate.get(ForeignTojos.Attribute.HASH.key());
    }

    /**
     * Checks if tojo was already optimized.
     *
     * @return True if optimization is required, false otherwise.
     */
    public boolean optimizationRequired(){
        final Path src = this.xmir();
        boolean res = true;
        if (this.delegate.exists(ForeignTojos.Attribute.XMIR_2.key())) {
            final Path tgt = this.xmirSecond();
            if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                Logger.debug(
                    this, "Already optimized %s to %s",
                    new Rel(src), new Rel(tgt)
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
    public boolean hasHash(){
        return this.delegate.exists(ForeignTojos.Attribute.HASH.key());
    }

    /**
     * Set the jar.
     * @param coordinates The coordinates of jar.
     * @return The tojo itself.
     */
    public ForeignTojo withJar(final Coordinates coordinates) {
        this.delegate.set(ForeignTojos.Attribute.JAR.key(), coordinates.toString());
        return this;
    }

    /**
     * Set the discovered size.
     * @param size The size.
     * @return The tojo itself.
     */
    public ForeignTojo withDiscovered(final int size) {
        this.delegate.set(ForeignTojos.Attribute.DISCOVERED.key(), Integer.valueOf(size));
        return this;
    }

    /**
     * Set the discovered at.
     * @param path The path where was discovered.
     * @return The tojo itself.
     */
    public ForeignTojo withDiscoveredAt(final Path path) {
        if (!this.delegate.exists(ForeignTojos.Attribute.VERSION.key())) {
            this.delegate.set(ForeignTojos.Attribute.VERSION.key(), "*.*.*");
        }
        this.delegate.set(ForeignTojos.Attribute.DISCOVERED_AT.key(), path);
        return this;
    }

    /**
     * Set sodg.
     * @param sodg Sodg.
     * @return The tojo itself.
     */
    public ForeignTojo withSodg(final Path sodg) {
        this.delegate.set(ForeignTojos.Attribute.SCOPE.key(), sodg.toString());
        return this;
    }

    /**
     * Set the xmir2.
     * @param xmir The xmir2.
     * @return The tojo itself.
     */
    public ForeignTojo withXmirSecond(final Path xmir) {
        this.delegate.set(ForeignTojos.Attribute.XMIR_2.key(), xmir.toString());
        return this;
    }
}
