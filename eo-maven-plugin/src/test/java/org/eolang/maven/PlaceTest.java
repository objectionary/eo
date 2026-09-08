/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Place}.
 *
 * @since 0.11
 */
final class PlaceTest {

    @Test
    void makesPath() {
        MatcherAssert.assertThat(
            "Place must make the path by full name of the object",
            new Place("hello.foo.bar")
                .make(Paths.get("/tmp/test"), MjAssemble.XMIR)
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo(String.format("/tmp/test/hello/foo/bar.%s", MjAssemble.XMIR))
        );
    }

    @Test
    void makesSimplePath() {
        MatcherAssert.assertThat(
            "Place must make the simple path",
            new Place("hey")
                .make(Paths.get("/tmp"), "xml")
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo("/tmp/hey.xml")
        );
    }
}
