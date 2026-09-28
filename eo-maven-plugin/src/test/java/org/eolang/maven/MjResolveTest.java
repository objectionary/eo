/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjResolve}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class MjResolveTest {

    @Test
    void reportsNoNewDependenciesWhenAllAreCached(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.7.0",
                "[] > main /bytes"
            )
        );
        maven.execute(new PpResolve());
        final List<String> messages = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                messages.add(String.valueOf(event.getRenderedMessage()));
            }

            @Override
            public void close() {
                // Nothing to release.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger logger = Logger.getLogger(Resolving.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            maven.execute(new PpResolve());
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        MatcherAssert.assertThat(
            "A cached dependency must not be reported as newly unpacked",
            messages,
            Matchers.hasItem("No new dependencies unpacked")
        );
    }

    @Test
    void resolvesWithSingleDependency(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.7.0",
            String.format("+version 0.25.0%n"),
            "[] > main /bytes"
            ).execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            temp
                .resolve("target")
                .resolve(MjResolve.DIR)
                .resolve("org.eolang/eo-runtime/-/0.7.0").resolve("eo-runtime-0.7.0.class")
                .toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void resolvesWhenPlaceDirectoryExistsButIsEmpty(@Mktmp final Path temp) throws IOException {
        final Path place = temp
            .resolve("target")
            .resolve(MjResolve.DIR)
            .resolve("org.eolang/eo-runtime/-/0.7.0");
        Files.createDirectories(place);
        new FakeMaven(temp).withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.7.0",
            String.format("+version 0.25.0%n"),
            "[] > main /bytes"
            ).execute(new PpResolve());
        MatcherAssert.assertThat(
            "An empty leftover directory from an interrupted unpack must not block re-resolving",
            place.resolve("eo-runtime-0.7.0.class").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void reportsMalformedRtJvmLocationClearly(@Mktmp final Path temp) throws IOException {
        final Path xmir = temp.resolve("dep.xmir");
        Files.writeString(
            xmir,
            String.join(
                "",
                "<object><metas><meta><head>rt</head>",
                "<tail>jvm org.eolang:eo-runtime</tail>",
                "<part>jvm</part><part>org.eolang:eo-runtime</part>",
                "</meta></metas></object>"
            )
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("dep").withXmir(xmir).withVersion("1.0.0");
        MatcherAssert.assertThat(
            "The error must name the malformed '+rt jvm' location",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new DpsDefault(tojos, false, false, false).iterator(),
                "A '+rt jvm' location with too few colon-separated parts must fail with a clear error, not an ArrayIndexOutOfBoundsException"
            ).getMessage(),
            Matchers.containsString("org.eolang:eo-runtime")
        );
    }

    @Test
    void reportsRtJvmLocationWithAnEmptyComponentClearly(@Mktmp final Path temp)
        throws IOException {
        final Path xmir = temp.resolve("dep.xmir");
        Files.writeString(
            xmir,
            String.join(
                "",
                "<object><metas><meta><head>rt</head>",
                "<tail>jvm org.eolang:eo-runtime:</tail>",
                "<part>jvm</part><part>org.eolang:eo-runtime:</part>",
                "</meta></metas></object>"
            )
        );
        final TjsForeign tojos = new TjsForeign();
        tojos.add("dep").withXmir(xmir).withVersion("1.0.0");
        MatcherAssert.assertThat(
            "The error must name the malformed '+rt jvm' location",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new DpsDefault(tojos, false, false, false).iterator(),
                "A '+rt jvm' location with an empty mandatory component, such as a missing version, must fail with a clear error, not register a dependency with a blank coordinate"
            ).getMessage(),
            Matchers.containsString("org.eolang:eo-runtime:")
        );
    }

    @Test
    void resolvesDefaultJnaDependency(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Default JNA dependency must be resolved",
            new FakeMaven(temp)
                .withHelloWorld()
                .with("ignoreRuntime", true)
                .execute(new PpResolve())
                .result(),
            Matchers.hasKey(String.format("target/%s/net.java.dev.jna/jna/-/5.14.0", MjResolve.DIR))
        );
    }

    @Test
    void resolvesWithoutAnyDependencies(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format("+package foo.x%n"),
            "[a b] > main",
            "  plus. > @",
            "    a",
            "    b"
        );
        maven.foreignTojos().add("sum");
        maven.execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            temp
                .resolve("target")
                .resolve(MjResolve.DIR)
                .resolve("org.eolang/eo-runtime/-/"),
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }

    @Test
    void resolvesWithEoRuntimeDependency(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld().execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }

    @Test
    void resolvesWithoutEoRuntimeDependency(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .with("ignoreRuntime", true)
            .execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must not exist, but it doesn't",
            maven.targetPath(),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.class"))
        );
    }

    @Test
    void resolvesIfRuntimeDependencyComesFromTojos(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.22.1",
            String.format("+version 0.25.0%n"),
            "[] > main"
        ).execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-0.22.1.class")
        );
    }

    @Test
    void resolvesIfRuntimeDependencyComesFromTojosButParamIsFalse(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                "",
                "[] > main"
            )
        ).with("ignoreRuntime", true).execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must not exist, but it doesn't",
            maven.targetPath(),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.class"))
        );
    }

    @Test
    void resolvesWithRuntimeDependencyFromPom(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Dependency runtime = new Dependency();
        runtime.setGroupId("org.eolang");
        runtime.setArtifactId("eo-runtime");
        runtime.setVersion("0.7.0");
        final MavenProject project = new MavenProject();
        project.setDependencies(Collections.singletonList(runtime));
        maven.withHelloWorld()
            .with("project", project)
            .execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-0.7.0.class")
        );
    }

    /**
     * Test conflicts.
     *
     * @param temp Temp folder
     * @throws IOException In case of I/O issues.
     */
    @Test
    void resolvesWithConflictingDependencies(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.22.1",
            String.format("+version 0.25.0%n"),
            "[] > main /bytes"
        ).withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.22.0",
            String.format("+version 0.25.0%n"),
            "[] > main-1 /bytes"
        );
        MatcherAssert.assertThat(
            "Expected that conflicting dependencies were found, but they were not",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> maven.execute(new PpResolve())
            ).getCause().getCause().getMessage(),
            Matchers.containsString(
                "1 conflicting dependencies are found: {org.eolang:eo-runtime:jar:=[0.22.0, 0.22.1]}"
            )
        );
    }

    @Test
    void resolvesWithConflictingDependenciesNoFail(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1",
                "",
                "[] > main /bytes"
            )
            ).withProgram(
                String.join(
                    System.lineSeparator(),
                    "+package foo.x",
                    "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1",
                    "",
                    "[] > main-1 /bytes"
                )
            );
        maven.with("ignoreConflicts", true)
            .execute(new PpResolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }

    @Test
    void keepsBothSiblingVersionsWithConflictsIgnored(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.22.0",
                String.format("+version 0.25.0%n"),
                "[] > main /bytes"
            )
        ).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                String.format("+version 0.25.0%n"),
                "[] > main-1 /bytes"
            )
        );
        maven.with("ignoreConflicts", true)
            .execute(new PpResolve());
        MatcherAssert.assertThat(
            "Both sibling versions must survive resolving, but one was deleted",
            maven.targetPath(),
            Matchers.allOf(
                new ContainsFiles("**/eo-runtime-0.22.0.class"),
                new ContainsFiles("**/eo-runtime-0.22.1.class")
            )
        );
    }
}
