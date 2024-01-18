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
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link JavaPath}.
 *
 * @since 0.29
 */
class JavaPathTest {

    @ParameterizedTest
    @CsvSource({
        "org.eolang, EOorg.EOeolang",
        "obj, EOobj",
        "obj.sub, EOobj.EOsub",
        "obj.sub$attr, EOobj.EOsub$EOattr",
        "obj.sub-dashed$attr, EOobj.EOsub_dashed$EOattr",
        "obj.sub-dashed$attr-dashed, EOobj.EOsub_dashed$EOattr_dashed",
        "'',''"
    })
    void convertsToString(final String name, final String expected) {
        MatcherAssert.assertThat(
            new JavaPath(name).toString(),
            Matchers.equalTo(expected)
        );
    }
}
