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
package org.eolang.maven.rust;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.eolang.maven.footprint.FtDefault;
import org.eolang.maven.util.HmBase;

/**
 * The class for storing and assigning names to
 * rust inserts by its location.
 * It loads names base from target and needs to
 * be saved for using in other process.
 * @since 0.30
 */
public final class Names {

    /**
     * Prefix for the names.
     */
    public static final String PREFIX = "native";

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final Path dest;

    /**
     * All names.
     */
    private final IoChecked<ConcurrentHashMap<String, String>> all;

    /**
     * Ctor.
     * @param target Directory where to serialize names.
     */
    public Names(final Path target) {
        this.dest = target;
        this.all = Names.checked(this.dest);
    }

    /**
     * Assign the new name to the function.
     * @param loc Location of the insert.
     * @return The name.
     */
    public String name(final String loc) {
        final Map<String, String> cached;
        try {
            cached = this.all.value();
        } catch (final IOException exc) {
            throw new IllegalArgumentException(
                String.format(
                    "Cannot load names correctly from %s",
                    this.dest
                ),
                exc
            );
        }
        return cached.computeIfAbsent(
            loc,
            key -> String.format(
                "%s%d",
                Names.PREFIX,
                cached.size()
            )
        );
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    @Override
    public boolean equals(final Object object) {
        if (this == object) {
            return true;
        }
        if (!(object instanceof Names)) {
            return false;
        }
        final Names names = (Names) object;
        try {
            return this.dest.equals(names.dest)
                && this.all.value().equals(names.all.value());
        } catch (final IOException exc) {
            throw new IllegalArgumentException(
                "Cannot load map correctly",
                exc
            );
        }
    }

    @Override
    public int hashCode() {
        return 31 * this.dest.hashCode()
            + 9719 * this.all.hashCode();
    }

    /**
     * Saves the function to name dispatching table.
     * @throws IOException If any issues with IO.
     */
    public void save() throws IOException {
        Files.createDirectories(this.dest.getParent());
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(this.all.value());
        oos.flush();
        new HmBase(this.dest.getParent()).save(
            new String(Base64.getEncoder().encode(baos.toByteArray()), StandardCharsets.UTF_8),
            this.dest.getFileName()
        );
    }

    /**
     * Prestructor to initialize this.all.
     * @param dest Directory where to load from.
     * @return Names map.
     */
    private static IoChecked<ConcurrentHashMap<String, String>> checked(final Path dest) {
        return new IoChecked<>(
            new Sticky<>(
                new Synced<>(
                    () -> {
                        final ConcurrentHashMap<String, String> result;
                        if (Files.exists(dest)) {
                            result = Names.load(dest);
                        } else {
                            result = new ConcurrentHashMap<>();
                        }
                        return result;
                    }
                )
            )
        );
    }

    /**
     * Loads the table.
     * @param src Path where to install from.
     * @return The map.
     * @throws IOException If any issues with IO.
     */
    @SuppressWarnings("unchecked")
    private static ConcurrentHashMap<String, String> load(final Path src) throws IOException {
        try (ObjectInputStream map = new ObjectInputStream(
            new ByteArrayInputStream(
                Base64.getDecoder().decode(
                    new FtDefault(src.getParent()).load(
                        src.getFileName().toString(),
                        ""
                    )
                )
            )
        )) {
            final Object result = map.readObject();
            if (result.getClass() != ConcurrentHashMap.class) {
                throw new ClassCastException(
                    String.format(
                        "Object inside %s has wrong class %s",
                        src,
                        result.getClass()
                    )
                );
            }
            return (ConcurrentHashMap<String, String>) result;
        } catch (final ClassNotFoundException exc) {
            throw new IllegalArgumentException(
                String.format(
                    "File %s contains invalid data",
                    src
                ),
                exc
            );
        }
    }
}
