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
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.stream.Collectors;
import javax.json.JsonReader;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonWriter;

/**
 * JSON file.
 *
 * The class is NOT thread-safe.
 *
 * @since 0.17
 */
public final class Json implements Mono {

    /**
     * The file path.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param path The path to the file
     */
    public Json(final Path path) {
        this.file = path;
    }

    @Override
    public Collection<Map<String, String>> read() throws IOException {
        final LinkedList<Map<String, String>> rows = new LinkedList<>();
        if (this.file.toFile().exists()) {
            try (
                Reader reader = Files.newBufferedReader(this.file);
                JsonReader json = javax.json.Json.createReader(reader)
            ) {
                json
                    .readArray()
                    .stream()
                    .map(Json::asMap)
                    .forEach(rows::add);
            }
        }
        return rows;
    }

    @Override
    public void write(final Collection<Map<String, String>> rows) throws IOException {
        this.file.toFile().getParentFile().mkdirs();
        try (
            Writer writer = Files.newBufferedWriter(this.file, StandardOpenOption.CREATE);
            JsonWriter json = javax.json.Json.createWriter(writer)
        ) {
            json.write(javax.json.Json.createArrayBuilder(rows).build());
        }
    }

    /**
     * Covert JsonValue to Map.
     * @param value Value
     * @return Map of Strings.
     */
    private static Map<String, String> asMap(final JsonValue value) {
        return value.asJsonObject()
            .entrySet()
            .stream()
            .collect(Collectors.toMap(Map.Entry::getKey, ent -> Json.asString(ent.getValue())));
    }

    /**
     * Convert JsonValue to String.
     * @param value JsonValue.
     * @return String.
     */
    private static String asString(final JsonValue value) {
        return JsonString.class.cast(value).getString();
    }

}
