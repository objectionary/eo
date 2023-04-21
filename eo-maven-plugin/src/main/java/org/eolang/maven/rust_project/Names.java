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

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import com.sun.tools.javac.util.Pair;
import org.eolang.maven.util.Home;
public final class Names {

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final Path dest;

    private final ConcurrentHashMap<String, String> all;

    private final String prefix;

    /**
     * Ctor
     * @param target Directory where to serialize names.
     * @throws IOException If any issues with IO.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public Names(final Path target) throws IOException {
        this.dest = target;
        this.prefix = target.toString().toLowerCase(Locale.ENGLISH).replaceAll("[^a-z0-9]", "x");
        if (this.dest.resolve("names.txt").toFile().exists()) {
            try {
                this.all = load(target.resolve("names.txt"));
            } catch (ClassNotFoundException exc) {
                throw new IllegalArgumentException(
                    String.format(
                        "File %s contains invalid data",
                        this.dest
                    ),
                    exc
                );
            }
        }
        else {
            this.all = new ConcurrentHashMap<>();
        }
    }

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

    public void save() throws IOException {
        Files.createDirectories(this.dest);
        new ObjectOutputStream(new FileOutputStream(this.dest.resolve("names.txt").toFile())).writeObject(this.all);
    }

    private static ConcurrentHashMap<String, String> load(final Path src) throws IOException, ClassNotFoundException {
        final String content = String.valueOf(new Home(src.getParent()).load(src.getFileName()));
        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(Base64.getDecoder().decode(content)));
        return (ConcurrentHashMap<String, String>) ois.readObject();
    }
}
