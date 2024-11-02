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

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.Saved;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link VerifyMojo}.
 *
 * @since 0.31.0
 * @todo #2674:30min The messages "Warnings identified" from
 *  /org/eolang/parser/fail-on-warnings.xsl
 *  can have nullable line number. Need fix it, that it works as in
 *  /org/eolang/parser/warnings/mandatory-version-meta.xsl and
 *  /org/eolang/parser/warnings/mandatory-home-meta.xsl.
 *  After you need fix {@code createRegEx()}.
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
final class VerifyMojoTest {

    @Test
    void doesNotFailWithNoErrorsAndWarnings(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Verify()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    void detectsErrorsSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "  QQ.io.stdout",
                "    \"Hello world\""
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Verify()),
            "Program with noname attributes should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Verified file should not exist",
            maven.result().get("target/6-verify/foo/x/main.xmir"),
            Matchers.equalTo(null)
        );
        MatcherAssert.assertThat(
            "Critical error must exist in shaken XMIR",
            new XMLDocument(
                maven.result().get("target/3-shake/foo/x/main.xmir")
            ).nodes("//errors/error[@severity='error']"),
            Matchers.hasSize(1)
        );
    }

    @Test
    void detectsCriticalErrorsSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "    \"Hello world\""
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Verify()),
            "Wrong program should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Verified file should not exist",
            maven.result().get("target/6-verify/foo/x/main.xmir"),
            Matchers.equalTo(null)
        );
        MatcherAssert.assertThat(
            "Error must exist in shaken XMIR",
            new XMLDocument(
                maven.result().get("target/3-shake/foo/x/main.xmir")
            ).nodes("//errors/error[@severity='critical']"),
            Matchers.hasSize(1)
        );
    }

    @Test
    void detectsWarningWithCorrespondingFlag(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[] > main",
                "  # This is the default 64+ symbols comment in front of named abstract object.",
                "  [] > @",
                "    \"Hello world\" > @"
            )
            .with("failOnWarning", true);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should have failed on warning, but it didn't"
        );
        MatcherAssert.assertThat(
            "Verified file should not exist",
            maven.result().get("target/6-verify/foo/x/main.xmir"),
            Matchers.equalTo(null)
        );
        MatcherAssert.assertThat(
            "Warning must exist in shaken XMIR",
            new XMLDocument(
                maven.result().get("target/3-shake/foo/x/main.xmir")
            ).nodes("//errors/error[@severity='warning']"),
            Matchers.hasSize(4)
        );
    }

    @Test
    void doesNotDetectWarningWithoutCorrespondingFlag(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "  # This is the default 64+ symbols comment in front of named abstract object.",
                    "  [] > @",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", false)
                .execute(new FakeMaven.Verify()),
            "Program with sparse decorated object should not have failed on warning without flag, but it does"
        );
    }

    @Test
    void failsOptimizationOnError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .execute(new FakeMaven.Verify()),
            "Error in the eo code because of invalid alias, should fail"
        );
    }

    @Test
    void failsOptimizationOnCritical(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                ).with("trackOptimizationSteps", true)
                .execute(new FakeMaven.Verify()),
            BinarizeParseTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsParsingOnError(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram("something > is wrong here")
                .execute(new FakeMaven.Verify()),
            "Program with invalid syntax should have failed, but it didn't"
        );
    }

    @Test
    void failsOnInvalidProgram(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(AssembleMojoTest.INVALID_PROGRAM)
                .execute(new FakeMaven.Verify()),
                "Invalid program with wrong syntax should have failed to assemble, but it didn't"
        );
    }

    @Test
    void failsOnWarning(@TempDir final Path temp) throws Exception {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+architect yegor256@gmail.com",
                    "+tests",
                    "+package org.eolang.examples\n",
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > main",
                    "  # This is the default 64+ symbols comment in front of named abstract object.",
                    "  [] > @",
                    "    hello > test"
                )
                .with("failOnWarning", true)
                .execute(new FakeMaven.Verify()),
            "Program with warning should fail"
        );
    }

    @Test
    void skipsAlreadyVerified(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Verify());
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", VerifyMojo.DIR, AssembleMojo.XMIR)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(VerifyMojo.class);
        MatcherAssert.assertThat(
            "VerifyMojo must skip verification if XMIR was already verified",
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void savesVerifiedResultsToCache(@TempDir final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache)
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Verify());
        MatcherAssert.assertThat(
            "Verified results must be saved to cache",
            cache.resolve(VerifyMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void getsAlreadyVerifiedResultsFromCache(@TempDir final Path temp) throws Exception {
        final TextOf cached = new TextOf(
            new ResourceOf("org/eolang/maven/optimize/main.xml")
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new Saved(
            cached,
            cache
                .resolve(VerifyMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(VerifyMojo.CACHE)
                    .resolve(FakeMaven.pluginVersion())
                    .resolve(hash)
                    .resolve("foo/x/main.xmir")
            ),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache)
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Verify());
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new XMLDocument(
                new HmBase(temp).load(
                    Paths.get(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            VerifyMojo.DIR,
                            AssembleMojo.XMIR
                        )
                    )
                ).asBytes()
            ),
            Matchers.is(new XMLDocument(cached.asString()))
        );
    }
}
