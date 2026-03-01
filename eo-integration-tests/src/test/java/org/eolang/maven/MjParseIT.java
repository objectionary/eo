/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:parse goal.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjParseIT {
    @Test
    @SuppressWarnings("PMD.UnitTestShouldIncludeAssert")
    void parsesSimpleFile(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    "# Simple object.\n[] > foo\n".getBytes(StandardCharsets.UTF_8)
                );
                new AppendedPlugin(f).value()
                    .goals("register", "parse");
                f.exec("compile", String.format("-Deo.cache=%s", temp.resolve("cache")));
                MatcherAssert.assertThat(
                    "the XMIR file is generated",
                    f.files().file("target/eo/1-parse/foo.xmir").exists(),
                    Matchers.is(true)
                );
            }
        );
    }
}
