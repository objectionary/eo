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
import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
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
@SuppressWarnings("PMD.TooManyMethods")
public final class ForeignTojos implements Closeable {

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
     * Add a foreign tojo.
     * @param name The name of the tojo.
     * @return The tojo.
     */
    public ForeignTojo add(final String name) {
        final Tojo tojo = this.tojos.value().add(name);
        if (!tojo.exists(Attribute.SCOPE.key())) {
            tojo.set(Attribute.SCOPE.key(), this.scope);
        }
        return new ForeignTojo(tojo);
    }

    /**
     * Get the tojos that are not discovered yet.
     * @return The tojos.
     */
    public Collection<ForeignTojo> notDiscovered() {
        return this.select(
            row -> row.exists(Attribute.OPTIMIZED.key()) && !row.exists(Attribute.DISCOVERED.key())
        );
    }

    /**
     * Get the tojos that have corresponding xmir.
     * @return The tojos.
     */
    public Collection<ForeignTojo> withXmir() {
        return this.select(row -> row.exists(Attribute.XMIR.key()));
    }

    /**
     * Get the tojos that doesn't have dependency.
     * @return The tojos.
     */
    public Collection<ForeignTojo> dependencies() {
        return this.select(
            t -> t.exists(Attribute.XMIR.key())
                && t.exists(Attribute.VERSION.key())
                && !t.exists(Attribute.JAR.key())
        );
    }

    /**
     * Get the tojos for the given eo.
     * @param source The eo object path.
     * @return The tojos.
     */
    public Collection<ForeignTojo> withSource(final Path source) {
        return this.select(
            row -> row.exists(Attribute.OPTIMIZED.key())
                && row.get(Attribute.EO.key()).equals(source.toString())
        );
    }

    /**
     * Get the tojos that have corresponding eo file.
     * @return The tojos.
     */
    public Collection<ForeignTojo> withSources() {
        return this.select(row -> row.exists(Attribute.EO.key()));
    }

    /**
     * Get the tojos that do not have corresponding eo and xmir.
     * @return The tojos.
     */
    public Collection<ForeignTojo> withoutSources() {
        return this.select(
            row -> !row.exists(Attribute.EO.key())
                && !row.exists(Attribute.XMIR.key())
        );
    }

    /**
     * Get the tojos that have corresponding optimized xmir.
     * @return The tojos.
     */
    public Collection<ForeignTojo> withOptimized() {
        return this.select(row -> row.exists(Attribute.OPTIMIZED.key()));
    }

    /**
     * Get the tojos that have not probed yet.
     * @return The tojos.
     */
    public Collection<ForeignTojo> unprobed() {
        return this.select(
            row -> row.exists(Attribute.OPTIMIZED.key())
                && !row.exists(Attribute.PROBED.key())
        );
    }

    /**
     * Get all tojos as a collection.
     * @return Collection of tojos.
     */
    public Collection<ForeignTojo> all() {
        return this.select(all -> true);
    }

    /**
     * Check if the tojos contains a foreign tojo with name.
     * @param name The name of the tojo.
     * @return True if the tojo exists.
     */
    public boolean contains(final String name) {
        return this.select(tojo -> tojo.get(Attribute.ID.key()).equals(name)).isEmpty();
    }

    /**
     * Get the size of the tojos.
     * @return The size of the tojos.
     */
    public int size() {
        return this.select(all -> true).size();
    }

    /**
     * Status of tojos.
     * @return Status in text
     */
    public String status() {
        final Attribute[] attrs = {
            Attribute.EO,
            Attribute.XMIR,
            Attribute.OPTIMIZED,
            Attribute.DISCOVERED,
            Attribute.PROBED,
        };
        final Collection<String> parts = new LinkedList<>();
        for (final Attribute attr : attrs) {
            parts.add(
                String.format(
                    "%s:%d",
                    attr,
                    this.select(tojo -> tojo.exists(attr.key())).size()
                )
            );
        }
        return String.join("/", parts);
    }

    /**
     * Select tojos.
     * @param filter Filter.
     * @return Selected tojos.
     */
    private Collection<ForeignTojo> select(final Predicate<? super Tojo> filter) {
        final Predicate<Tojo> scoped = t -> t.get(Attribute.SCOPE.key()).equals(this.scope);
        final Predicate<Tojo> test = t -> "test".equals(this.scope);
        return this.tojos.value()
            .select(t -> filter.test(t) && (scoped.test(t) || test.test(t)))
            .stream().map(ForeignTojo::new).collect(Collectors.toList());
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
         * Path to the optimized xmir file.
         */
        OPTIMIZED("optimized"),

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
