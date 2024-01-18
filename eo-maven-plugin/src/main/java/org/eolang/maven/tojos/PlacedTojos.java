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
import org.eolang.maven.Catalogs;
import org.eolang.maven.util.FileHash;

/**
 * PlacedTojos encapsulates tojos logic and keeps short information about all placed files.
 *
 * @since 0.30
 */
public final class PlacedTojos implements Closeable {

    /**
     * All tojos.
     */
    private final Unchecked<? extends Tojos> all;

    /**
     * Ctor.
     * @param file Path to the tojos file.
     */
    public PlacedTojos(final Path file) {
        this(Catalogs.INSTANCE.make(file));
    }

    /**
     * Ctor.
     * @param tojos Tojos source.
     */
    public PlacedTojos(final Sticky<? extends Tojos> tojos) {
        this(new Unchecked<>(tojos));
    }

    /**
     * Ctor.
     * @param tojos Tojos.
     */
    private PlacedTojos(final Tojos tojos) {
        this(new Sticky<>(() -> tojos));
    }

    /**
     * The main ctor.
     * @param tojos Tojos unchecked source.
     */
    private PlacedTojos(final Unchecked<? extends Tojos> tojos) {
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
    public Collection<PlacedTojo> classes() {
        return this.allBinaries().stream()
            .filter(PlacedTojo::isClass)
            .collect(Collectors.toList());
    }

    /**
     * Get all jars.
     * @return All jars.
     */
    public Collection<PlacedTojo> jars() {
        return this.allBinaries().stream()
            .filter(PlacedTojo::isJar)
            .collect(Collectors.toList());
    }

    /**
     * Get all binaries.
     * @return All binaries jars with classes.
     */
    public List<PlacedTojo> allBinaries() {
        return this.all.value()
            .select(tojos -> true)
            .stream()
            .map(PlacedTojo::new)
            .collect(Collectors.toList());
    }

    /**
     * Find jar by dependency identifier.
     * @param dep Dependency identifier.
     * @return Placed jar.
     */
    public Optional<PlacedTojo> findJar(final String dep) {
        return this.jars().stream()
            .filter(tojo -> tojo.identifier().equals(dep))
            .findFirst();
    }

    /**
     * Find placed tojo by path.
     * @param target Path.
     * @return Placed tojo.
     */
    public Optional<PlacedTojo> find(final Path target) {
        return this.allBinaries().stream().filter(
            tojo -> target.toString().equals(tojo.identifier())
        ).findFirst();
    }

    /**
     * Place class into placed tojos file.
     * @param target Path to the class.
     * @param related Related.
     * @param dep Dependency.
     * @return Placed class.
     */
    public PlacedTojo placeClass(
        final Path target,
        final String related,
        final String dep
    ) {
        return new PlacedTojo(
            this.all.value().add(target.toString())
                .set(Attribute.KIND.key(), "class")
                .set(Attribute.HASH.key(), new FileHash(target))
                .set(Attribute.RELATED.key(), related)
                .set(Attribute.DEPENDENCY.key(), dep)
                .set(Attribute.UNPLACED.key(), "false")
        );
    }

    /**
     * Place jar into placed tojos file.
     * @param name Name of the jar.
     */
    public void placeJar(final String name) {
        this.all.value().add(name)
            .set(Attribute.KIND.key(), "jar")
            .set(Attribute.DEPENDENCY.key(), String.format("%s.jar", name))
            .set(Attribute.UNPLACED.key(), "false");
    }

    /**
     * Unplace all tojos.
     */
    public void unplaceAll() {
        this.allBinaries().forEach(PlacedTojo::unplace);
    }

    /**
     * Check whether tojos is empty.
     * @return True if empty.
     */
    public boolean isEmpty() {
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
        String key() {
            return this.key;
        }
    }
}
