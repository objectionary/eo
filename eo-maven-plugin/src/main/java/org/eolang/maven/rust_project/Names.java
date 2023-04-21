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
package org.eolang.maven.rust_project;

import com.sun.tools.javac.util.Pair;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import org.eolang.maven.util.Home;

/**
 * Uniquely converts the loc into the name for jni function.
 * @since 0.29
 */
public final class Names {

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final Path dest;

    /**
     * All names.
     */
    private final ConcurrentHashMap<String, String> all;

    /**
     * Prefix for the name.
     */
    private final String prefix;

    /**
     * Ctor.
     * @param target Directory where to serialize names.
     * @throws IOException If any issues with IO.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public Names(final Path target) throws IOException {
        this.dest = target.resolve("names.txt");
        this.prefix = target.toString().toLowerCase(Locale.ENGLISH).replaceAll("[^a-z0-9]", "x");
        if (this.dest.toFile().exists()) {
            this.all = this.load(this.dest);
        } else {
            this.all = new ConcurrentHashMap<>();
        }
    }

    /**
     * Assign the new name to the function.
     * @param code Code of the function.
     * @param dependencies Function dependencies.
     * @return The name.
     */
    public String name(final String code, final String dependencies) {
        return this.all.computeIfAbsent(
            new Pair<>(code, dependencies).toString(),
            key -> String.format(
                "%s%d",
                this.prefix,
                this.all.size()
            )
        );
    }

    /**
     * Saves the function to name dispatching table.
     * @throws IOException If any issues with IO.
     */
    public void save() throws IOException {
        Files.createDirectories(this.dest.getParent());
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(this.all);
        oos.close();
        new Home(this.dest.getParent()).save(
            Base64.getEncoder().encodeToString(baos.toByteArray()),
            this.dest.getFileName()
        );
    }

    /**
     * Loads the table.
     * @param src Path where to install from.
     * @return The map.
     * @throws IOException If any issues with IO.
     */
    private static ConcurrentHashMap<String, String> load(final Path src) throws IOException {
        final String content = String.valueOf(new Home(src.getParent()).load(src.getFileName()));
        final ObjectInputStream ois = new ObjectInputStream(
            new ByteArrayInputStream(Base64.getDecoder().decode(content))
        );
        try {
            return (ConcurrentHashMap<String, String>) ois.readObject();
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
