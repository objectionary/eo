/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test cases for {@link PhiMojo}.
 * @since 0.34.0
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
final class PhiMojoTest {
    /**
     * Comment.
     */
    private static final String COMMENT =
        "# This is the default 64+ symbols comment in front of named abstract object.";

    @Test
    @Disabled
    void convertsSimpleObjectToPhi(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    String.format("%s\n[] > foo\n", PhiMojoTest.COMMENT).getBytes()
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("register", "parse", "optimize", "xmir-to-phi");
                f.exec("compile");
                MatcherAssert.assertThat(
                    "the .phi file is generated",
                    f.files().file("target/eo/phi/foo.phi").exists(),
                    Matchers.is(true)
                );
                f.files().show();
            }
        );
    }

    @Test
    void createsFiles(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            String.format(
                "There' should be file with .%s extension after translation to phi, but there isn't",
                PhiMojo.EXT
            ),
            new FakeMaven(temp)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > cart",
                    "  memory 0 > total",
                    "  [i] > add",
                    "    total.write > @",
                    "      total.plus i"
                )
                .execute(new FakeMaven.Phi())
                .result(),
            Matchers.hasKey(String.format("target/phi/foo/x/main.%s", PhiMojo.EXT))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi/xmir", glob = "**.xmir")
    void convertsXmirsToPhiWithoutCriticalErrorsWithoutOptimizations(
        final String xmir,
        @Mktmp final Path temp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("phiFailOnError", false)
            .with("phiOptimize", false);
        new HmBase(temp).save(xmir, Paths.get("target/2-optimize/test.xmir"));
        Assertions.assertDoesNotThrow(
            () -> maven.execute(PhiMojo.class),
            BinarizeParseTest.TO_ADD_MESSAGE
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi/xmir", glob = "**.xmir")
    void convertsXmirsToPhiWithoutCriticalErrorsWithOptimizations(
        final String xmir,
        @Mktmp final Path temp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("phiFailOnError", false)
            .with("phiOptimize", true);
        new HmBase(temp).save(xmir, Paths.get("target/2-optimize/test.xmir"));
        Assertions.assertDoesNotThrow(
            () -> maven.execute(PhiMojo.class),
            BinarizeParseTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void doesNotFailOnCritical(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("phiFailOnCritical", false)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > with-duplicates",
                    "  true > x",
                    "  false > x"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on critical errors with 'phiFailsOnCritical' = false"
        );
    }

    @Test
    void skipsFailedOnCriticalError(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("phiFailOnCritical", true)
                .with("phiSkipFailed", true)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > with-duplicates",
                    "  true > x",
                    "  false > x"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on critical errors with 'phiSkipFailed' = true"
        );
    }

    @Test
    void failsOnError(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > without-name",
                    "  true"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should fail on errors with 'phiFailOnError' = true"
        );
    }

    @Test
    void doesNotFailOnError(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("phiFailOnError", false)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > without-name",
                    "  true"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on errors with 'phiFailOnError' = false"
        );
    }

    @Test
    void skipsFailedOnError(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("phiFailOnCritical", false)
                .with("phiFailOnError", true)
                .with("phiSkipFailed", true)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > without-name",
                    "  true"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on errors with 'phiSkipFailed' = true"
        );
    }

    @Test
    void doesNotSaveSkippedFile(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Skipped file should not be saved after PhiMojo is done",
            new FakeMaven(temp)
                .with("phiFailOnCritical", true)
                .with("phiSkipFailed", true)
                .withProgram(
                    PhiMojoTest.COMMENT,
                    "[] > with-duplicates",
                    "  true > x",
                    "  false > x"
                )
                .execute(new FakeMaven.Phi())
                .result(),
            Matchers.not(
                Matchers.hasKey(
                    String.format("target/phi/foo/x/main.%s", PhiMojo.EXT)
                )
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi/yaml", glob = "**.yaml")
    void checksPhiPacks(final String pack, @Mktmp final Path temp) throws Exception {
        final Map<String, Object> map = new Yaml().load(pack);
        if (map.get("skip") != null) {
            Assumptions.abort(
                String.format("%s is not ready", pack)
            );
        }
        MatcherAssert.assertThat(
            String.format(
                "Result phi expression should be equal to %s, but it doesn't",
                map.get("phi").toString()
            ),
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(map.get("eo").toString())
                    .execute(new FakeMaven.Phi())
                    .result()
                    .get("target/phi/foo/x/main.phi")
            ).asString(),
            Matchers.equalTo(map.get("phi").toString())
        );
    }
}
