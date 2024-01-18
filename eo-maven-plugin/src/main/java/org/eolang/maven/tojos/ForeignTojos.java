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

import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.TjSmart;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.name.ObjectName;

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
    private final Unchecked<? extends Tojos> tojos;

    /**
     * Scope.
     */
    private final Supplier<String> scope;

    /**
     * Ctor.
     * @param scalar Scalar
     * @param scope Scope
     */
    public ForeignTojos(final Scalar<Tojos> scalar, final Supplier<String> scope) {
        this(new Unchecked<>(new Sticky<>(scalar)), scope);
    }

    /**
     * Constructor for tests.
     * Keeps all tojos in memory.
     */
    ForeignTojos() {
        this(() -> new TjSmart(new TjCached(new TjDefault(new MnMemory()))));
    }

    /**
     * Ctor with the default scope.
     * @param scalar Scalar
     */
    private ForeignTojos(final Scalar<Tojos> scalar) {
        this(scalar, () -> "compile");
    }

    /**
     * Main constructor.
     * @param tojos The tojos.
     * @param scope The scope.
     */
    private ForeignTojos(
        final Unchecked<Tojos> tojos,
        final Supplier<String> scope
    ) {
        this.tojos = tojos;
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
            tojo.set(Attribute.SCOPE.key(), this.scope.get());
        }
        return new ForeignTojo(tojo);
    }

    /**
     * Add a foreign tojo.
     * @param name The name of the tojo as {@link ObjectName}.
     * @return The tojo.
     */
    public ForeignTojo add(final ObjectName name) {
        return this.add(name.toString());
    }

    /**
     * Find tojo by tojo id.
     * @param id The id of the tojo.
     * @return The tojo.
     */
    public ForeignTojo find(final String id) {
        return new ForeignTojo(
            this.tojos.value()
                .select(tojo -> tojo.get(Attribute.ID.key()).equals(id))
                .stream()
                .findFirst()
                .orElseThrow(
                    () -> new IllegalArgumentException(
                        String.format("Tojo '%s' not found", id)
                    )
                )
        );
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
        return !this.select(tojo -> tojo.get(Attribute.ID.key()).equals(name)).isEmpty();
    }

    /**
     * Check if the tojos contains a foreign tojos with object name.
     * @param name The name of the tojo.
     * @return True if tojo exists.
     */
    public boolean contains(final ObjectName... name) {
        return Arrays.stream(name).map(Object::toString).allMatch(this::contains);
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
        final Predicate<Tojo> scoped = t -> t.get(Attribute.SCOPE.key()).equals(this.scope.get());
        return this.tojos.value()
            .select(t -> filter.test(t) && scoped.test(t))
            .stream().map(ForeignTojo::new).collect(Collectors.toList());
    }

    /**
     * Foreign tojo attributes.
     */
    enum Attribute {

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
         * Path to the shaken xmir file.
         */
        SHAKEN("shaken"),

        /**
         * Path to the verified xmir file.
         */
        VERIFIED("verified"),

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
         * How many objects were probed in the tojo.
         * Let's consider the next eo code:
         * <p>
         * {@code
         * [] > main
         *   QQ.io.stdout > @
         *     QQ.txt.sprintf "I am %d years old"
         *       plus.
         *         1337
         *         228
         * }
         * </p>
         * <p>In this code there are 5 objects that were probed:</p>
         *  - "org.eolang"
         *  - "org.eolang.io"
         *  - "org.eolang.txt"
         *  - "org.eolang.io.stdout"
         *  - "org.eolang.txt.sprintf"
         * <p>For more info see {@link org.eolang.maven.ProbeMojo}. </p>
         */
        PROBED("probed"),

        /**
         * Scope.
         */
        SCOPE("scope"),

        /**
         * Hash.
         * Object version hash from git.
         */
        HASH("hash"),

        /**
         * Ver.
         */
        VER("ver");

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
        String key() {
            return this.key;
        }
    }
}
