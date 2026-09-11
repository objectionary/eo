/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Xml}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class XmlTest {

    @Test
    void savesNodeIntoNestedFile(@Mktmp final Path temp) throws IOException {
        final Path target = temp.resolve("a").resolve("b.xmir");
        new Xml(new Xnav("<object><o name='λ'>S7</o></object>").element("object").node())
            .saved(target);
        MatcherAssert.assertThat(
            "the node must be spelled into the file, but it wasnt",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.containsString("<o name=\"λ\">S7</o>")
        );
    }

    @Test
    void keepsEmojiAsItIs() {
        MatcherAssert.assertThat(
            "the emoji of a test attribute must be spelled as it is, but it was escaped",
            new Xml(new Xnav("<object><o name='p🌵t'/></object>").element("object").node()).text(),
            Matchers.containsString("name=\"p🌵t\"")
        );
    }
}
