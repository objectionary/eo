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
package org.eolang.maven;

import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PhiTranslatorMojo}.
 *
 * @since 0.32
 * @todo #2535:30min Enable tests: {@link PhiTranslatorMojoTest#checksExistence(java.nio.file.Path)}
 *  and {@link PhiTranslatorMojoTest#checksPhiCalculusExpression(java.nio.file.Path)}.
 *  Also it's better to add more test cases in different EO programs with corresponding Phi-calculus
 *  expressions.
 */
class PhiTranslatorMojoTest {

    /**
     * Generate phi-calculus expression by XMIR.
     *
     * @param temp Temporary directory.
     * @throws Exception
     */
    @Test
    @Disabled
    void checksExistence(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram(
                    "[] > cart",
                    "  memory 0 > total",
                    "  [i] > add",
                    "    total.write > @",
                    "      total.plus i"
                )
                .execute(new FakeMaven.PhiTranslator())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/cart.%s", PhiTranslatorMojo.DIR, PhiTranslatorMojo.EXT)
            )
        );
    }

    @Test
    @Disabled
    void checksPhiCalculusExpression(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram(
                    "[] > holder",
                    "  103 > storage"
                )
                .execute(new FakeMaven.PhiTranslator())
                .result()
                .get(
                    String.format(
                        "target/%s/holder.%s",
                        PhiTranslatorMojo.DIR,
                        PhiTranslatorMojo.EXT
                    )
                )
                .toFile()
                .toString(),
            Matchers.equalTo(
                "holder ↦ ⟦ storage ↦ 103 ⟧"
            )
        );
    }
}
