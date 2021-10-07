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

import java.io.IOException;
import java.util.Collection;
import java.util.Map;

/**
 * One file-object.
 *
 * The class is NOT thread-safe.
 *
 * @since 0.12
 */
final class CsvTojo implements Tojo {

    /**
     * The file.
     */
    private final Csv csv;

    /**
     * The name.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param file The CSV
     * @param nme The name
     */
    CsvTojo(final Csv file, final String nme) {
        this.csv = file;
        this.name = nme;
    }

    @Override
    public boolean exists(final String key) throws IOException {
        return this.csv.read().stream()
            .filter(row -> row.get("id").equals(this.name))
            .findFirst()
            .get()
            .containsKey(key);
    }

    @Override
    public String get(final String key) throws IOException {
        final String value = this.csv.read().stream()
            .filter(row -> row.get("id").equals(this.name))
            .findFirst()
            .get()
            .get(key);
        if (value == null) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s' key", key
                )
            );
        }
        return value;
    }

    @Override
    public Tojo set(final String key, final String value) throws IOException {
        final Collection<Map<String, String>> rows = this.csv.read();
        final Map<String, String> row = rows.stream().filter(
            r -> r.get("id").equals(this.name)
        ).findFirst().get();
        row.put(key, value);
        this.csv.write(rows);
        return this;
    }
}
