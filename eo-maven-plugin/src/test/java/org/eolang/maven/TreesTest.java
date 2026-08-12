/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Random;
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
    void servesTreeRememberedByAnotherObject(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final String mark = String.format("метка-%d", new Random(seed).nextInt(1000));
        final String text = String.format("# Комментарий %d.%n[] > obj%n  42 > @%n", seed);
        new Trees.TsSaved(tmp).remember(
            text, new XMLDocument(String.format("<object mark=\"%s\"/>", mark))
        );
        MatcherAssert.assertThat(
            String.format("the tree one goal walked did not reach the next one, seed %d", seed),
            new Trees.TsSaved(tmp).tree(text, UnaryOperator.identity()).xpath("/object/@mark"),
            Matchers.contains(mark)
        );
    }
}
