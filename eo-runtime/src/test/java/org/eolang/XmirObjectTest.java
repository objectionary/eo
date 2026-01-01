/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package org.eolang;

import com.google.common.reflect.ClassPath;
import java.io.IOException;
import java.lang.reflect.Modifier;
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
        MatcherAssert.assertThat(
            "All top-level EOxx classes must be public",
            ClassPath.from(ClassLoader.getSystemClassLoader())
                .getAllClasses()
                .stream()
                .filter(
                    clazz -> "EOorg.EOeolang".equals(clazz.getPackageName())
                        && clazz.getSimpleName().startsWith("EO")
                )
                .map(ClassPath.ClassInfo::load)
                .filter(
                    clazz -> !Modifier.isPublic(clazz.getModifiers())
                        && !(clazz.isMemberClass() || clazz.isLocalClass())
                        && Phi.class.isAssignableFrom(clazz)
                )
                .collect(Collectors.toList()),
            Matchers.empty()
        );
    }
}
