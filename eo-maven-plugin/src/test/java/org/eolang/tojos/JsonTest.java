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
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link Json}.
 *
 * @since 0.17
 */
public final class JsonTest {

    @Test
    public void simpleScenario(@TempDir final Path temp) throws IOException {
        final Mono json = new Json(temp.resolve("foo/bar/a.json"));
        final Collection<Map<String, String>> rows = json.read();
        MatcherAssert.assertThat(
            json.read().size(),
            Matchers.equalTo(0)
        );
        final Map<String, String> row = new HashMap<>(0);
        final String key = "id";
        final String value = "привет,\t\r\n друг!";
        row.put(key, value);
        rows.add(row);
        json.write(rows);
        MatcherAssert.assertThat(
            json.read().iterator().next().get(key),
            Matchers.equalTo(value)
        );
    }

    @Test
    public void writesEmptyCollection(@TempDir final Path temp) throws IOException {
        final Mono json = new Json(temp.resolve("foo/bar/b.json"));
        json.write(Collections.emptyList());
        MatcherAssert.assertThat(
            json.read(),
            Matchers.empty()
        );
    }

}
