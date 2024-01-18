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
package org.eolang.maven.dependencies;

import com.yegor256.WeAreOnline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DcsWithRuntime}.
 *
 * @since 0.28.11
 */
final class DcsWithRuntimeTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsHardcodedVersionOfRuntimeDependency() {
        MatcherAssert.assertThat(
            new DcsWithRuntime(
                new DcsFake(5),
                DcsFake.runtimeDep()
            ),
            Matchers.iterableWithSize(6)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsRemoteVersionOfRuntimeDependency() {
        MatcherAssert.assertThat(
            new DcsWithRuntime(
                new DcsFake(2)
            ),
            Matchers.iterableWithSize(3)
        );
    }
}
