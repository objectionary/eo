/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.TjSmart;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * Foreign tojos.
 *
 * @since 0.30
 */
@SuppressWarnings("PMD.TooManyMethods")
final class TjsForeign implements Closeable {

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
    TjsForeign(final Scalar<Tojos> scalar, final Supplier<String> scope) {
        this(new Unchecked<>(new Synced<>(new Sticky<>(scalar))), scope);
    }

    /**
     * Constructor for tests.
     * Keeps all tojos in memory.
     */
    TjsForeign() {
        this(() -> new TjSmart(new TjCached(new TjDefault(new MnMemory()))));
    }

    /**
     * Ctor with the default scope.
     * @param scalar Scalar
     */
    private TjsForeign(final Scalar<Tojos> scalar) {
        this(scalar, () -> "compile");
    }

    /**
     * Main constructor.
     * @param tojos The tojos.
     * @param scope The scope.
     */
    private TjsForeign(
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
    TjForeign add(final String name) {
        final Tojo tojo = this.tojos.value().add(name);
        if (!tojo.exists(Attribute.SCOPE.getKey())) {
            tojo.set(Attribute.SCOPE.getKey(), this.scope.get());
        }
        return new TjForeign(tojo);
    }

    /**
     * Find tojo by tojo id.
     * @param id The id of the tojo.
     * @return The tojo.
     */
    TjForeign find(final String id) {
        return new TjForeign(
            this.tojos.value()
                .select(tojo -> tojo.get(Attribute.ID.getKey()).equals(id))
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
     * Get the tojos that have corresponding xmir.
     * @return The tojos.
     */
    Collection<TjForeign> withXmir() {
        return this.select(row -> row.exists(Attribute.XMIR.getKey()));
    }

    /**
     * Get the tojos that doesn't have dependency.
     * @return The tojos.
     */
    Collection<TjForeign> dependencies() {
        return this.select(
            t -> t.exists(Attribute.XMIR.getKey())
                && t.exists(Attribute.VERSION.getKey())
                && !t.exists(Attribute.JAR.getKey())
        );
    }

    /**
     * Get the tojos that have corresponding eo file.
     * @return The tojos.
     */
    Collection<TjForeign> withSources() {
        return this.select(row -> row.exists(Attribute.EO.getKey()));
    }

    /**
     * Get the tojos that do not have corresponding eo and xmir.
     * @return The tojos.
     */
    Collection<TjForeign> withoutSources() {
        return this.select(
            row -> !row.exists(Attribute.EO.getKey())
                && !row.exists(Attribute.XMIR.getKey())
        );
    }

    /**
     * Get the tojos that have not probed yet.
     * @return The tojos.
     */
    Collection<TjForeign> unprobed() {
        return this.select(
            row -> row.exists(Attribute.XMIR.getKey()) && !row.exists(Attribute.PROBED.getKey())
        );
    }

    /**
     * Get all tojos as a collection.
     * @return Collection of tojos.
     */
    Collection<TjForeign> all() {
        return this.select(all -> true).stream()
            .sorted(Comparator.comparing(TjForeign::identifier))
            .collect(Collectors.toList());
    }

    /**
     * Check if the tojos contains a foreign tojo with name.
     * @param name The name of the tojo.
     * @return True if the tojo exists.
     */
    boolean contains(final String name) {
        return !this.select(tojo -> tojo.get(Attribute.ID.getKey()).equals(name)).isEmpty();
    }

    /**
     * Get the size of the tojos.
     * @return The size of the tojos.
     */
    int size() {
        return this.select(all -> true).size();
    }

    /**
     * Status of tojos.
     * @return Status in text
     */
    String status() {
        final Attribute[] attrs = {
            Attribute.EO,
            Attribute.XMIR,
            Attribute.PROBED,
        };
        final Collection<String> parts = new LinkedList<>();
        for (final Attribute attr : attrs) {
            parts.add(
                String.format(
                    "%s:%d",
                    attr,
                    this.select(tojo -> tojo.exists(attr.getKey())).size()
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
    private Collection<TjForeign> select(final Predicate<? super Tojo> filter) {
        final Predicate<Tojo> scoped = t ->
            t.get(Attribute.SCOPE.getKey()).equals(this.scope.get());
        return this.tojos.value()
            .select(t -> filter.test(t) && scoped.test(t))
            .stream().map(TjForeign::new).collect(Collectors.toList());
    }

    /**
     * Foreign tojo attributes.
     */
    enum Attribute {

        /**
         * FQN of the object, e.g. {@code org.eolang.number}.
         */
        ID("id"),

        /**
         * Absolute path of the {@code .eo} file.
         */
        EO("eo"),

        /**
         * Version of eo.
         */
        VERSION("version"),

        /**
         * Absolute path of the {@code .xmir} file generated by the parser.
         */
        XMIR("xmir"),

        /**
         * Absolute path of the verified {@code .xmir} file.
         */
        LINTED("linted"),

        /**
         * Absolute path of the JAR file where the {@code .class} file for this EO program
         * has been found.
         */
        JAR("jar"),

        /**
         * Absolute path of the {@code .xmir} file where this object was discovered.
         */
        DISCOVERED_AT("discovered-at"),

        /**
         * How many objects were probed in the tojo.
         * Let's consider the next eo code:
         * <p>
         * {@code
         * [] > main
         *   QQ.io.stdout > @
         *     QQ.tt.sprintf "I am %d years old"
         *       plus.
         *         1337
         *         228
         * }
         * </p>
         * <p>In this code there are 5 objects that were probed:</p>
         *  - "org.eolang"
         *  - "org.eolang.io"
         *  - "org.eolang.tt"
         *  - "org.eolang.io.stdout"
         *  - "org.eolang.tt.sprintf"
         * <p>For more info see {@link MjProbe}. </p>
         */
        PROBED("probed"),

        /**
         * The scope of compilation, either {@code compile} or {@code test}.
         */
        SCOPE("scope"),

        /**
         * Git SHA of the object in the {@code objectionary/home}.
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
        String getKey() {
            return this.key;
        }
    }
}
