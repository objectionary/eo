/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package org.eolang;

import com.google.common.reflect.ClassPath;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.util.Set;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link XmirObject}.
 *
 * @since 0.38
 */
final class XmirObjectTest {

    @Test
    void annotatesOnlyPublicClasses() throws IOException {
        final Set<Class<?>> clazzes =  ClassPath.from(ClassLoader.getSystemClassLoader())
            .getAllClasses()
            .stream()
            .filter(clazz -> "EOorg.EOeolang".equals(clazz.getPackageName()))
            .map(ClassPath.ClassInfo::load)
            .filter(
                clazz -> clazz.getSimpleName().startsWith("EO")
                    && Phi.class.isAssignableFrom(clazz)
            )
            .collect(Collectors.toSet());
        assert !clazzes.isEmpty();
        MatcherAssert.assertThat(
            "Some EOxx classes are found",
            clazzes.stream()
                .filter(clazz -> !Modifier.isPublic(clazz.getModifiers()))
                .collect(Collectors.toList()),
            Matchers.empty()
        );
        MatcherAssert.assertThat(
            "All EOxx classes are public",
            clazzes.stream()
                .filter(clazz -> !Modifier.isPublic(clazz.getModifiers()))
                .collect(Collectors.toList()),
            Matchers.empty()
        );
    }

}
