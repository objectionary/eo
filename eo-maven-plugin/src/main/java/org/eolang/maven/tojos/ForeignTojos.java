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
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * Foreign tojos.
 *
 * @since 0.30
 */
public final class ForeignTojos implements Tojos {

    /**
     * The delegate.
     */
    private final Unchecked<Tojos> tojos;

    /**
     * Scope.
     */
    private final String scope;

    /**
     * Ctor.
     * @param scalar Scalar
     */
    public ForeignTojos(final Scalar<Tojos> scalar) {
        this(scalar, "compile");
    }

    /**
     * Ctor.
     * @param scalar Scalar
     * @param scope Scope
     */
    public ForeignTojos(final Scalar<Tojos> scalar, final String scope) {
        this.tojos = new Unchecked<>(new Sticky<>(scalar));
        this.scope = scope;
    }

    @Override
    public void close() throws IOException {
        this.tojos.value().close();
    }

    /**
     * Get the tojos that doesn't have dependency.
     * @return The tojos.
     */
    public Collection<ForeignTojo> dependencies() {
        return this.tojos.value()
            .select(
                t -> t.exists(Attribute.XMIR.key())
                    && t.exists(Attribute.VERSION.key())
                    && !t.exists(Attribute.JAR.key())
            ).stream()
            .map(ForeignTojo::new)
            .collect(Collectors.toList());
    }

    /**
     * Add a foreign tojo.
     * Similar to {@link ForeignTojos#add(String)}, but returns {@link ForeignTojo}.
     * @param name The name of the tojo.
     * @return The tojo.
     */
    public ForeignTojo addForeign(final String name) {
        return new ForeignTojo(this.add(name));
    }

    @Override
    public Tojo add(final String name) {
        final Tojo tojo = this.tojos.value().add(name);
        if (!tojo.exists(Attribute.SCOPE.key())) {
            tojo.set(Attribute.SCOPE.key(), this.scope);
        }
        return tojo;
    }

    @Override
    public List<Tojo> select(final Predicate<Tojo> filter) {
        return this.tojos.value().select(
            t -> filter.test(t)
                && (t.get(Attribute.SCOPE.key()).equals(this.scope) || "test".equals(this.scope)
            )
        );
    }

    /**
     * Get the tojos for the given eo.
     * @param eobj The eo object path.
     * @return The tojos.
     */
    public Collection<ForeignTojo> forEo(final Path eobj) {
        return this.tojos.value()
            .select(
                row -> row.exists(Attribute.XMIR_2.key())
                    && row.get(Attribute.EO.key()).equals(eobj.toString())
            ).stream()
            .map(ForeignTojo::new)
            .collect(Collectors.toList());
    }

    /**
     * Get the tojos that are not discovered yet.
     * @return The tojos.
     */
    public Collection<ForeignTojo> isNotDiscoveredYet() {
        return this.tojos.value()
            .select(
                row -> row.exists(Attribute.XMIR_2.key()) && !row.exists(Attribute.DISCOVERED.key())
            ).stream()
            .map(ForeignTojo::new)
            .collect(Collectors.toList());
    }

    /**
     * Get the size of the tojos.
     * @return The size of the tojos.
     */
    public int size() {
        return this.tojos.value().select(all -> true).size();
    }

    /**
     * Get the old tojos.
     * @return The old tojos.
     */
    public Tojos value() {
        return this.tojos.value();
    }

    /**
     * Foreign tojo attributes.
     */
    public enum Attribute {

        /**
         * Tojo id.
         */
        ID("id"),

        /**
         * Tojo eo file.
         */
        EO("eo"),

        /**
         * Version of eo.
         */
        VERSION("version"),

        /**
         * Tojo xmir file.
         */
        XMIR("xmir"),

        /**
         * Tojo xmir2 file.
         * @todo #1969:30min Add more meaningful name for the xmir2 attribute.
         *  I believe that instead of xmir2 we have to invent something more meaningful and
         *  change all related methods and parameters that use xmir2 abbreviation.
         */
        XMIR_2("xmir2"),

        /**
         * Absolute location of SODG file.
         */
        SODG("sodg"),

        /**
         * Absolute location of JAR file.
         */
        JAR("jar"),

        /**
         * Discovered.
         */
        DISCOVERED("discovered"),

        /**
         * Where this object was discovered.
         */
        DISCOVERED_AT("discovered-at"),

        /**
         * Probed.
         */
        PROBED("probed"),

        /**
         * Scope.
         */
        SCOPE("scope"),

        /**
         * Transpiled.
         */
        TRANSPILED("transpiled"),

        /**
         * Hash.
         */
        HASH("hash");

        /**
         * Attribute name.
         */
        private final String key;

        /**
         * Ctor.
         * @param attribute The attribute name.
         */
        Attribute(final String attribute) {
            this.key = attribute;
        }

        /**
         * Get the attribute name.
         * @return The attribute name.
         */
        public String key() {
            return this.key;
        }
    }
}
