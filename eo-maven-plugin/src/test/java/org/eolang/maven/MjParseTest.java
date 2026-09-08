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
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.HexOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.Canonical;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjParse}.
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class MjParseTest {

    @Test
    void parsesSuccessfully(@Mktmp final Path temp) throws Exception {
        final String parsed = String.format(
            "target/%s/foo/x/main.%s",
            Parsing.DIR,
            MjAssemble.XMIR
        );
        MatcherAssert.assertThat(
            String.format("ParseMojo should have parsed stdout object %s, but didn't", parsed),
            new FakeMaven(temp).withHelloWorld()
                .execute(new PpParse())
                .result(),
            Matchers.hasKey(parsed)
        );
    }

    @Test
    void parsesSuccessfullyAndSavesToForeign(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld().execute(new PpParse()).result();
        MatcherAssert.assertThat(
            "The resource must exist, but it doesn't",
            maven.foreign().getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    void parsesAgainWhatTheMergeReplaced(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[n] > foo", "  n > @"),
            "foo",
            "foo.eo"
        ).withProgram(
            String.join(System.lineSeparator(), "+package foo", "", "[] > bar", "  42 > @"),
            "foo.bar",
            "foo/bar.eo"
        );
        maven.execute(new PpMerge()).execute(MjParse.class);
        MatcherAssert.assertThat(
            "the merged XMIR cannot stand for a parsed one, or the next build lints the merge",
            maven.foreignTojos().find("foo").xmir().toString(),
            Matchers.endsWith(Paths.get(Parsing.DIR).resolve("foo.xmir").toString())
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
    void parsesWithCache(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram("invalid content")
            .with("cache", cache.toFile());
        final String expected = new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
        ).asString();
        final CommitHash hash = new ChCached(new ChNarrow(new ChRemote("0.40.5")));
        final Path base = maven.targetPath().resolve(Parsing.DIR);
        final Path target = new Place("foo.x.main").make(base, MjAssemble.XMIR);
        new Cache(
            cache.resolve(Parsing.CACHE)
                .resolve(MjParseTest.cacheVersion(maven.programTojo().identifier()))
                .resolve(hash.value()),
            src -> expected
        ).apply(maven.programTojo().source(), target, base.relativize(target));
        target.toFile().delete();
        final String actual = String.format(
            "target/%s/foo/x/main.%s",
            Parsing.DIR,
            MjAssemble.XMIR
        );
        MatcherAssert.assertThat(
            String.format("We expect that that %s is taken from the cache, but it didn't", actual),
            new TextOf(
                maven.allTojosWithHash(hash)
                    .execute(new PpParse())
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
            new FakeMaven(temp).withProgram(
                String.format("+package foo.x%n"),
                "[] > main",
                "  seq *-1 > @",
                "    true"
                )
                .execute(new PpParse())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", Parsing.DIR, MjAssemble.XMIR)
            )
        );
    }

    @Test
    void homesBareReferenceToSamePackageObject(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo",
                "",
                "[] > bar",
                "  42 > @"
            ),
            "foo.bar",
            "foo/bar.eo"
        ).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo",
                "",
                "[] > app",
                "  bar > @"
            ),
            "foo.app",
            "foo/app.eo"
        );
        MatcherAssert.assertThat(
            "bare reference 'bar' must be resolved into the same package as 'Φ.foo.bar'",
            new XMLDocument(
                maven.execute(new PpParse()).result().get(
                    String.format("target/%s/foo/app.%s", Parsing.DIR, MjAssemble.XMIR)
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='Φ.foo.bar']")
        );
    }

    @Test
    void keepsBareGlobalReferenceAtRoot(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo",
                "",
                "[] > app",
                "  bar > @"
            ),
            "foo.app",
            "foo/app.eo"
        );
        MatcherAssert.assertThat(
            "bare reference 'bar' must stay at the root when no 'Φ.foo.bar' object exists",
            new XMLDocument(
                maven.execute(new PpParse()).result().get(
                    String.format("target/%s/foo/app.%s", Parsing.DIR, MjAssemble.XMIR)
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='Φ.bar']")
        );
    }

    @Test
    void crashesIfWrongPackage(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package wrong.package",
                "",
                "[] > hello",
                "  42 > @"
            )
        ).execute(new PpParse());
        MatcherAssert.assertThat(
            "The XMIR with broken content must exist, but it doesn't",
            new WkDefault(temp.resolve("target")).stream().anyMatch(
                path -> path.toFile().getName().startsWith("broken-")
                    && path.toFile().getName().endsWith(".xmir")
            ),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotParseIfAlreadyParsed(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> result = maven
            .withHelloWorld()
            .execute(new PpParse())
            .result();
        final File parsed = result.get(
            String.format("target/%s/foo/x/main.%s", Parsing.DIR, MjAssemble.XMIR)
        ).toFile();
        final long before = parsed.lastModified();
        maven.execute(MjParse.class);
        MatcherAssert.assertThat(
            "File was modified",
            before,
            Matchers.equalTo(parsed.lastModified())
        );
    }

    @Test
    void parsesAgainWhenTheSourceChangesUnderTheSameTimestamp(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path parsed = maven
            .withHelloWorld()
            .execute(new PpParse())
            .result()
            .get(String.format("target/%s/foo/x/main.%s", Parsing.DIR, MjAssemble.XMIR));
        final Path source = temp.resolve(Paths.get("foo", "x", "main.eo"));
        new Saved(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "",
                "[greeting] > main",
                "  greeting > @"
            ),
            source
        ).value();
        Files.setLastModifiedTime(source, Files.getLastModifiedTime(parsed));
        maven.execute(MjParse.class);
        MatcherAssert.assertThat(
            "the XMIR stayed as it was, while the source it came from had changed",
            new XMLDocument(parsed),
            XhtmlMatchers.hasXPaths("//o[@name='greeting']")
        );
    }

    /**
     * The test with high number of eo programs reveals concurrency problems of the ParseMojo.
     * Since other tests works only with single program - it's hard to find concurrency mistakes.
     * @param temp Test directory
     * @throws IOException If problem with filesystem happened.
     */
    @Test
    void parsesConcurrentlyWithLotsOfPrograms(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 50;
        final Collection<String> xmirs = new ArrayList<>(total);
        for (int program = 0; program < total; ++program) {
            maven.withProgram(
                String.format("+package foo.x%n%n[] > main%s", FakeMaven.suffix(program))
            );
            xmirs.add(
                String.format(
                    "target/%s/foo/x/main%s.%s",
                    Parsing.DIR,
                    FakeMaven.suffix(program),
                    MjAssemble.XMIR
                )
            );
        }
        MatcherAssert.assertThat(
            "All programs must be parsed in a single concurrent run, but some XMIRs are absent",
            maven.execute(new PpParse()).result().keySet(),
            Matchers.hasItems(xmirs.toArray(new String[0]))
        );
    }

    @Test
    void addsErrorsIfObjectNameDoesNotMatchFilename(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Errors are not present in the resulted XMIR, but they should",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    "[] > app",
                    "main"
                    )
                    .execute(new PpParse())
                    .result()
                    .get(String.format("target/%s/main.%s", Parsing.DIR, MjAssemble.XMIR))
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
                    .execute(new PpParse())
                    .result()
                    .get("target/1-parse/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPaths(
                "//error[@severity='critical']",
                "//error[@check='mandatory-object-name'][contains(text(), 'found 0')]"
            )
        );
    }

    @Test
    void parsesWithTargetCache(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final File parsed = maven
            .withHelloWorld()
            .execute(new PpParse())
            .result().get(String.format("target/%s/foo/x/main.%s", Parsing.DIR, MjAssemble.XMIR))
            .toFile();
        Files.setLastModifiedTime(
            parsed.toPath(), FileTime.fromMillis(System.currentTimeMillis() + 60_000L)
        );
        final long before = parsed.lastModified();
        maven.execute(new PpParse());
        final long after = parsed.lastModified();
        maven.withProgram(
            String.join(
                System.lineSeparator(),
                "[] > foo",
                "  boom > @"
            ),
            "foo",
            "foo/x/foo.eo"
        ).execute(new PpParse());
        MatcherAssert.assertThat(
            "Parser re-parsed sources, but it should not",
            parsed.lastModified(),
            Matchers.allOf(
                Matchers.equalTo(before),
                Matchers.equalTo(after)
            )
        );
    }

    private static String cacheVersion(final String identifier) {
        return String.format(
            "%s-%s-%s",
            FakeMaven.pluginVersion(),
            new Fingerprint(
                Stream.concat(
                    Canonical.XSLS.stream(), Canonical.IMPORTS.stream()
                ).toArray(String[]::new)
            ).get(),
            new UncheckedText(
                new HexOf(new Sha256DigestOf(new InputOf(identifier)))
            ).asString()
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
