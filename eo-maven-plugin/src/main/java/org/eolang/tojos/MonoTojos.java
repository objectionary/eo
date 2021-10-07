/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
package org.eolang.tojos;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.cactoos.Func;
import org.cactoos.func.IoCheckedFunc;

/**
 * All file-objects.
 *
 * The class is NOT thread-safe.
 *
 * @since 0.12
 */
public final class MonoTojos implements Tojos {

    /**
     * The mono.
     */
    private final Mono mono;

    /**
     * Ctor.
     *
     * @param path The path to the file
     */
    public MonoTojos(final File path) {
        this(path.toPath());
    }

    /**
     * Ctor.
     *
     * @param path The path to the file
     */
    public MonoTojos(final Path path) {
        this(new Csv(path));
    }

    /**
     * Ctor.
     *
     * @param mno The CSV
     */
    public MonoTojos(final Mono mno) {
        this.mono = mno;
    }

    @Override
    public int size() throws IOException {
        return this.mono.read().size();
    }

    @Override
    public Tojo add(final String name) throws IOException {
        final Collection<Map<String, String>> rows = this.mono.read();
        final Optional<Map<String, String>> before = rows.stream().filter(
            r -> r.get("id").equals(name)
        ).findFirst();
        if (!before.isPresent()) {
            final Map<String, String> row = new HashMap<>(1);
            row.put("id", name);
            rows.add(row);
            this.mono.write(rows);
        }
        return new MonoTojo(this.mono, name);
    }

    @Override
    public Collection<Tojo> select(final Func<Tojo, Boolean> filter) throws IOException {
        final Collection<Map<String, String>> rows = this.mono.read();
        final Collection<Tojo> tojos = new ArrayList<>(rows.size());
        final IoCheckedFunc<Tojo, Boolean> safe = new IoCheckedFunc<>(filter);
        for (final Map<String, String> row : rows) {
            final Tojo tojo = new MonoTojo(this.mono, row.get("id"));
            if (safe.apply(tojo)) {
                tojos.add(tojo);
            }
        }
        return tojos;
    }
}
