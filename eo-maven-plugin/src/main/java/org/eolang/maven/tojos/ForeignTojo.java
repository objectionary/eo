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

import com.yegor256.tojos.Tojo;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.Coordinates;

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
     * Set the jar.
     * @param coordinates The coordinates of jar.
     */
    public void withJar(final Coordinates coordinates) {
        this.delegate.set(ForeignTojos.Attribute.JAR.key(), coordinates.toString());
    }
}
