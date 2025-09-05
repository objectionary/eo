/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for {@link MjPrint}.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjPrintIT {
    @Test
    void printsSimpleObject(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        " ",
                        "# This unit test is supposed to check",
                        "the functionality of the corresponding object.\n[] > foo\n"
                    ).getBytes()
                );
                new AppendedPlugin(f).value()
                    .goals("register", "parse");
                f.exec("compile");
                f.files()
                    .file("src/main/xmir/foo.xmir")
                    .save(
                        f.files()
                            .file(
                                String.format("target/eo/%s/foo.xmir", MjParse.DIR)
                            ).path()
                    );
                f.exec("eo:print");
                MatcherAssert.assertThat(
                    "the .eo file is generated",
                    f.files().file("target/generated-sources/eo/foo.eo").exists(),
                    Matchers.is(true)
                );
            }
        );
    }
}
