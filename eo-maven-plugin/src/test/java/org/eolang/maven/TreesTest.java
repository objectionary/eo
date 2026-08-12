/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.Random;
import java.util.function.UnaryOperator;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Node;

/**
 * Test for {@link Trees}.
 * @since 0.74
 */
final class TreesTest {

    @Test
    void dontLeakOnePipelineIntoTheNext() throws IOException {
        final long seed = System.nanoTime();
        final String text = String.format(
            "# Комментарий %d.%n[] > obj%d%n  42 > @%n", seed, new Random(seed).nextInt(1000)
        );
        final Trees trees = new Trees.TsShared();
        trees.tree(
            text,
            xml -> {
                final Node root = new Xnav(xml.inner()).element("object").node();
                root.getParentNode().removeChild(root);
                return xml;
            }
        );
        MatcherAssert.assertThat(
            String.format("the remembered tree was damaged by an earlier pipeline, seed %d", seed),
            trees.tree(text, UnaryOperator.identity()).nodes("/object").size(),
            Matchers.equalTo(1)
        );
    }
}
