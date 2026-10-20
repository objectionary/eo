/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.yegor256.xsline.TrDefault;
import org.cactoos.io.InputOf;
import org.junit.jupiter.api.Test;

final class ZProbeTest {
    @Test
    void probe() throws Exception {
        for (final String src : new String[] {
            "[] > app\n  I > s\n    5\n",
            "[] > app\n  (x > [x]) > s\n    5\n",
            "[] > app\n  foo > s\n    5\n",
        }) {
            System.out.println("=== " + src.replace('\n', '|'));
            final XML out = new EoSyntax(new InputOf(src), new TrDefault<>()).parsed();
            System.out.println(out.nodes("/object").get(0).toString()
                .replaceAll("(?s)<listing>.*?</listing>", "")
                .replaceAll("\\s+\n", "\n"));
        }
    }
}
