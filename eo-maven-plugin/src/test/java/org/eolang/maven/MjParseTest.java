/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjParse}.
 *
 * @since 0.1
 */
@SuppressWarnings({
    "PMD.AvoidDuplicateLiterals",
    "PMD.TooManyMethods"
})
@ExtendWith(MktmpResolver.class)
final class MjParseTest {

    @Test
    void parsesSuccessfully(@Mktmp final Path temp) throws Exception {
        final String parsed = String.format(
            "target/%s/foo/x/main.%s",
            MjParse.DIR,
            MjAssemble.XMIR
        );
        MatcherAssert.assertThat(
            String.format("ParseMojo should have parsed stdout object %s, but didn't", parsed),
            new FakeMaven(temp).withHelloWorld()
                .execute(new FakeMaven.Parse())
                .result(),
            Matchers.hasKey(parsed)
        );
    }

    @Test
    void parsesSuccessfullyAndSavesToForeign(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld().execute(new FakeMaven.Parse()).result();
        MatcherAssert.assertThat(
            "The resource must exist, but it doesn't",
            maven.foreign().getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    void failsOnTimeout(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .with("timeout", 0)
                .execute(Infinite.class),
            "Expected IllegalStateException on timeout"
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void parsesWithCache(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram("invalid content")
            .with("cache", cache.toFile());
        final String expected = new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
        ).asString();
        final CommitHash hash = new ChCached(new ChNarrow(new ChRemote("0.40.5")));
        final Path base = maven.targetPath().resolve(MjParse.DIR);
        final Path target = new Place("foo.x.main").make(base, MjAssemble.XMIR);
        new Cache(
            cache.resolve(MjParse.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash.value()),
            src -> expected
        ).apply(maven.programTojo().source(), target, base.relativize(target));
        target.toFile().delete();
        final String actual = String.format(
            "target/%s/foo/x/main.%s",
            MjParse.DIR,
            MjAssemble.XMIR
        );
        MatcherAssert.assertThat(
            String.format("We expect that that %s is taken from the cache, but it didn't", actual),
            new TextOf(
                maven.allTojosWithHash(hash)
                    .execute(new FakeMaven.Parse())
                    .result()
                    .get(actual)
            ).toString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void doesNotCrashesOnError(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
            new FakeMaven(temp)
                .withProgram(
                    "+package foo.x\n",
                    "# Error.",
                    "[] > main",
                    "  seq *-1 > @",
                    "    true"
                )
                .execute(new FakeMaven.Parse())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", MjParse.DIR, MjAssemble.XMIR)
            )
        );
    }

    @Test
    void crashesIfWrongPackage(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package wrong.package\n",
                "# Hello.",
                "[] > hello",
                "  42 > @"
            )
            .execute(new FakeMaven.Parse());
        MatcherAssert.assertThat(
            "The XMIR with broken content must exist, but it doesn't",
            new Walk(temp.resolve("target")).stream().anyMatch(
                path -> path.toFile().getName().startsWith("broken-")
                    && path.toFile().getName().endsWith(".xmir")
            ),
            Matchers.is(true)
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void doesNotParseIfAlreadyParsed(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> result = maven
            .withHelloWorld()
            .execute(new FakeMaven.Parse())
            .result();
        final File parsed = result.get(
            String.format("target/%s/foo/x/main.%s", MjParse.DIR, MjAssemble.XMIR)
        ).toFile();
        final long before = parsed.lastModified();
        maven.execute(MjParse.class);
        final long after = parsed.lastModified();
        MatcherAssert.assertThat(
            "File was modified",
            before,
            Matchers.equalTo(after)
        );
    }

    /**
     * The test with high number of eo programs reveals concurrency problems of the ParseMojo.
     * Since other tests works only with single program - it's hard to find concurrency mistakes.
     * @param temp Test directory.
     * @throws IOException If problem with filesystem happened.
     */
    @Test
    void parsesConcurrentlyWithLotsOfPrograms(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 50;
        for (int program = 0; program < total; ++program) {
            maven.withProgram(
                String.format("+package foo.x\n\n# Program\n[] > main%s", FakeMaven.suffix(program))
            );
        }
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                "We have to parse concurrently, but we didn't",
                maven.execute(new FakeMaven.Parse()).result(),
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        MjParse.DIR,
                        FakeMaven.suffix(program),
                        MjAssemble.XMIR
                    )
                )
            );
        }
    }

    @Test
    void addsErrorsIfObjectNameDoesNotMatchFilename(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Errors are not present in the resulted XMIR, but they should",
            new XMLDocument(
                new FakeMaven(temp)
                    .withProgram(
                        "# App.\n[] > app",
                        "main"
                    )
                    .execute(new FakeMaven.Parse())
                    .result()
                    .get(String.format("target/%s/main.%s", MjParse.DIR, MjAssemble.XMIR))
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors[count(error)=1]",
                "//error[@severity='critical']",
                "//error[contains(text(), \"does not match the name of the object\")]"
            )
        );
    }

    @Test
    void addsErrorsWhenObjectNameFails(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Errors are not present in the resulted XMIR, but they should",
            new XMLDocument(
                new FakeMaven(temp)
                    .withProgram("# App.")
                    .execute(new FakeMaven.Parse())
                    .result()
                    .get("target/1-parse/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPaths(
                "//error[@severity='critical']",
                "//error[text()=\"XMIR should have either '/object/o/@name' or '/object/class/@name' attribute\"]"
            )
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void parsesWithTargetCache(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final File parsed = maven
            .withHelloWorld()
            .execute(new FakeMaven.Parse())
            .result().get(String.format("target/%s/foo/x/main.%s", MjParse.DIR, MjAssemble.XMIR))
            .toFile();
        Files.setLastModifiedTime(
            parsed.toPath(), FileTime.fromMillis(System.currentTimeMillis() + 60_000L)
        );
        final long before = parsed.lastModified();
        maven.execute(new FakeMaven.Parse());
        final long after = parsed.lastModified();
        maven.withProgram(
            String.join(
                "\n",
                "[] > foo",
                "  boom > @"
            ),
            "foo",
            "foo/x/foo.eo"
        ).execute(new FakeMaven.Parse());
        MatcherAssert.assertThat(
            "Parser re-parsed sources, but it should not",
            parsed.lastModified(),
            Matchers.allOf(
                Matchers.equalTo(before),
                Matchers.equalTo(after)
            )
        );
    }

    /**
     * The mojo that does nothing, but executes infinitely.
     * @since 0.29
     */
    @Mojo(name = "infinite", defaultPhase = LifecyclePhase.VALIDATE)
    private static final class Infinite extends MjSafe {
        @Override
        public void exec() {
            try {
                Thread.sleep(Long.MAX_VALUE);
            } catch (final InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException(ex);
            }
        }
    }
}
