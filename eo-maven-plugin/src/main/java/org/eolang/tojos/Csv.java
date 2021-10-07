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
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * CSV file.
 *
 * The class is NOT thread-safe.
 *
 * @since 0.12
 */
public final class Csv implements Mono {

    /**
     * The file where to keep them.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param path The path to the file
     */
    public Csv(final Path path) {
        this.file = path;
    }

    @Override
    public Collection<Map<String, String>> read() throws IOException {
        final Collection<Map<String, String>> rows = new LinkedList<>();
        if (Files.exists(this.file)) {
            for (final String line : Files.readAllLines(this.file, StandardCharsets.UTF_8)) {
                final Map<String, String> row = new HashMap<>(1);
                final String[] cols = line.split("\t");
                for (final String part : cols) {
                    final String[] parts = part.split(":", 2);
                    row.put(Csv.decode(parts[0]), Csv.decode(parts[1]));
                }
                rows.add(row);
            }
        }
        return rows;
    }

    @Override
    public void write(final Collection<Map<String, String>> rows) throws IOException {
        final Collection<String> lines = new ArrayList<>(rows.size());
        for (final Map<String, String> row : rows) {
            final Collection<String> cols = new ArrayList<>(row.size());
            for (final Map.Entry<String, String> ent : row.entrySet()) {
                cols.add(
                    String.format(
                        "%s:%s",
                        Csv.encode(ent.getKey()),
                        Csv.encode(ent.getValue())
                    )
                );
            }
            lines.add(String.join("\t", cols));
        }
        this.file.toFile().getParentFile().mkdirs();
        Files.write(this.file, lines, StandardCharsets.UTF_8);
    }

    /**
     * Encode.
     * @param txt The text to encode
     * @return Encoded
     */
    private static String encode(final String txt) {
        try {
            return URLEncoder.encode(txt, StandardCharsets.UTF_8.toString());
        } catch (final UnsupportedEncodingException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Decode.
     * @param txt The text to decode
     * @return Decoded
     */
    private static String decode(final String txt) {
        try {
            return URLDecoder.decode(txt, StandardCharsets.UTF_8.toString());
        } catch (final UnsupportedEncodingException ex) {
            throw new IllegalStateException(ex);
        }
    }

}
