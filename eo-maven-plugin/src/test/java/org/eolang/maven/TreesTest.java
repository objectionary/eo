/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.function.UnaryOperator;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link Trees}.
 * @since 0.74
 */
final class TreesTest {

    @Test
    void servesTreeToAnotherObjectAsItWasWalked(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format("# Комментарий %d.%n[] > obj%n  42 > @%n", seed);
        final String walked = "<object mark=\"метка\"><o name=\"obj\"><o base=\"x\"/></o></object>";
        new Trees.TsSaved(tmp).remember(text, new XMLDocument(walked));
        MatcherAssert.assertThat(
            String.format("the tree the next goal got is not the walked one, seed %d", seed),
            new Trees.TsSaved(tmp).tree(text, UnaryOperator.identity()).toString(),
            Matchers.equalTo(new XMLDocument(walked).toString())
        );
    }
}
