/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for {@link SodgMojo}.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class SodgMojoIT {
    @Test
    void convertsSimpleObjectToGraph(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("XMIR.xsd").save(
                    Paths.get(System.getProperty("user.dir")).resolve(
                        "../eo-parser/src/main/resources/XMIR.xsd"
                    )
                );
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        " ",
                        "# This unit test is supposed to check the functionality",
                        "of the corresponding object.\n[] > foo\n"
                    ).getBytes()
                );
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty(
                            "eo.version",
                            Manifests.read("EO-Version")
                        )
                    )
                    .execution()
                    .goals("register", "parse", "shake", "sodg");
                f.exec("compile");
                MatcherAssert.assertThat(
                    "the .sodg file is generated",
                    f.files().file("target/eo/sodg/foo.sodg").exists(),
                    Matchers.is(true)
                );
            }
        );
    }
}
