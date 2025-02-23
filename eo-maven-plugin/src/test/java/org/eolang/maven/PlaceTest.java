/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
            CatalogsTest.TO_ADD_MESSAGE,
            new Place("hello.foo.bar")
                .make(Paths.get("/tmp/test"), AssembleMojo.XMIR)
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo(String.format("/tmp/test/hello/foo/bar.%s", AssembleMojo.XMIR))
        );
    }

    @Test
    void makesSimplePath() {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new Place("hey")
                .make(Paths.get("/tmp"), "xml")
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo("/tmp/hey.xml")
        );
    }
}
