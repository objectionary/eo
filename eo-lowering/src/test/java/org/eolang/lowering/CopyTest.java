/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Paths;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests of the class {@link Copy}.
 *
 * @since 0.64.0
 */
final class CopyTest {

    @Test
    void placesACopyUnderTheDirectoriesOfItsPackage() throws Exception {
        MatcherAssert.assertThat(
            "the copy must go under the directories of its package, but it doesnt",
            new Copy(
                Paths.get("w", "app.xmir"),
                new EoSyntax(String.format("+package foo.bar%n%n[a] > app%n  a > @%n")).parsed()
            ).relative(),
            Matchers.equalTo(Paths.get("foo", "bar", "app.xmir"))
        );
    }

    @Test
    void placesACopyWithNoPackageAtTheTop() throws Exception {
        MatcherAssert.assertThat(
            "the copy of an object with no package must stay at the top, but it doesnt",
            new Copy(
                Paths.get("w", "app.xmir"),
                new EoSyntax(String.format("[a] > app%n  a > @%n")).parsed()
            ).relative(),
            Matchers.equalTo(Paths.get("app.xmir"))
        );
    }
}
