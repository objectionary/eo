/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * PlacedTojos encapsulates tojos logic and keeps short information about all placed files.
 *
 * @since 0.30
 */
final class TjsPlaced implements Closeable {

    /**
     * All tojos.
     */
    private final Unchecked<? extends Tojos> all;

    /**
     * Ctor.
     * @param file Path to the tojos file.
     */
    TjsPlaced(final Path file) {
        this(Catalogs.INSTANCE.make(file));
    }

    /**
     * Ctor.
     * @param tojos Tojos source.
     */
    TjsPlaced(final Sticky<? extends Tojos> tojos) {
        this(new Unchecked<>(tojos));
    }

    /**
     * Ctor.
     * @param tojos Tojos.
     */
    private TjsPlaced(final Tojos tojos) {
        this(new Sticky<>(() -> tojos));
    }

    /**
     * The main ctor.
     * @param tojos Tojos unchecked source.
     */
    private TjsPlaced(final Unchecked<? extends Tojos> tojos) {
        this.all = tojos;
    }

    @Override
    public void close() throws IOException {
        this.all.value().close();
    }

    /**
     * Get all classes.
     * @return All classes.
     */
    Collection<TjPlaced> classes() {
        return this.allBinaries()
            .stream()
            .filter(TjPlaced::isClass)
            .collect(Collectors.toList());
    }

    /**
     * Get all jars.
     * @return All jars.
     */
    Collection<TjPlaced> jars() {
        return this.allBinaries()
            .stream()
            .filter(TjPlaced::isJar)
            .collect(Collectors.toList());
    }

    /**
     * Get all binaries.
     * @return All binaries jars with classes.
     */
    List<TjPlaced> allBinaries() {
        return this.all.value()
            .select(tojos -> true)
            .stream()
            .map(TjPlaced::new)
            .collect(Collectors.toList());
    }

    /**
     * Find jar by dependency identifier.
     * @param dep Dependency identifier.
     * @return Placed jar.
     */
    Optional<TjPlaced> findJar(final String dep) {
        return this.jars()
            .stream()
            .filter(tojo -> tojo.identifier().equals(dep))
            .findFirst();
    }

    /**
     * Find placed tojo by path.
     * @param target Path.
     * @return Placed tojo.
     */
    Optional<TjPlaced> find(final Path target) {
        return this.allBinaries().stream().filter(
            tojo -> target.toString().equals(tojo.identifier())
        ).findFirst();
    }

    /**
     * Place class into placed tojos file.
     * @param target Path to the class.
     * @param related Related.
     * @param dep Dependency.
     */
    void placeClass(
        final Path target,
        final String related,
        final String dep
    ) {
        this.all.value()
            .add(target.toString())
            .set(Attribute.KIND.getKey(), "class")
            .set(Attribute.HASH.getKey(), new FileHash(target))
            .set(Attribute.RELATED.getKey(), related)
            .set(Attribute.DEPENDENCY.getKey(), dep)
            .set(Attribute.UNPLACED.getKey(), "false");
    }

    /**
     * Unplace all tojos.
     */
    void unplaceAll() {
        this.allBinaries().forEach(TjPlaced::unplace);
    }

    /**
     * Check whether tojos is empty.
     * @return True if empty.
     */
    boolean isEmpty() {
        return this.all.value().select(row -> true).isEmpty();
    }

    /**
     * Placed tojo attributes.
     *
     * @since 0.30
     */
    enum Attribute {

        /**
         * Tojo id.
         */
        ID("id"),

        /**
         * Tojo kind.
         */
        KIND("kind"),

        /**
         * Tojo dependency.
         */
        DEPENDENCY("dependency"),

        /**
         * Tojo hash.
         */
        HASH("hash"),

        /**
         * Tojo related.
         */
        RELATED("related"),

        /**
         * Tojo unplaced.
         */
        UNPLACED("unplaced");

        /**
         * Tojo attribute inside placed tojo.
         */
        private final String key;

        /**
         * Ctor.
         * @param attribute Key in a file.
         */
        Attribute(final String attribute) {
            this.key = attribute;
        }

        /**
         * Get attribute key.
         * @return Key.
         */
        String getKey() {
            return this.key;
        }
    }
}
