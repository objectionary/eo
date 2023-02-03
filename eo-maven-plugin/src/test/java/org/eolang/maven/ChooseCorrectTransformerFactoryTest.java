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

import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.xml.transform.TransformerFactory;
import net.sf.saxon.TransformerFactoryImpl;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for choosing correct implementation of {@link javax.xml.transform.TransformerFactory}.
 *
 * @since 0.28.12
 */
class ChooseCorrectTransformerFactoryTest {
    @Test
    void choosesOnce() {
        MatcherAssert.assertThat(
            TransformerFactory.newInstance().getClass(),
            Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
        );
    }

    @Test
    void choosesCorrectlyInConcurrentEnvironment() {
        for (final Class<? extends TransformerFactory> clazz : IntStream.range(0, 100).parallel()
            .mapToObj(i -> TransformerFactory.newInstance().getClass())
            .collect(Collectors.toList())) {
            MatcherAssert.assertThat(
                clazz,
                Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
            );
        }
    }
}
