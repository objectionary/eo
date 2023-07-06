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
package org.eolang.maven.tojos;

import java.io.IOException;
import java.nio.file.Path;
import org.eolang.maven.FakeMaven;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Tests from {@link ForeignTojos}.
 *
 * @since 0.29.5
 */
final class ForeignTojosTest {

    /**
     * Testable foreign tojos.
     */
    private ForeignTojos tojos;

    @BeforeEach
    void setUp(@TempDir final Path tmp) {
        this.tojos = new ForeignTojos(() -> new FakeMaven(tmp).foreign());
    }

    @ParameterizedTest
    @CsvSource({
        "abs",
        "org.eolang.int",
        "QQ.io.stdout"
    })
    void contains(final String name) {
        this.tojos.add(name);
        MatcherAssert.assertThat(
            this.tojos.contains(name),
            Matchers.is(true)
        );
    }

    @AfterEach
    void tearDown() throws IOException {
        this.tojos.close();
    }
}
