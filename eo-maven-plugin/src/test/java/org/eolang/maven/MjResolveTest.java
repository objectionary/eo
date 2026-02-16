/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;
import org.cactoos.Func;
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
@SuppressWarnings({
    "PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods",
    "PMD.UnnecessaryLocalRule", "PMD.UnitTestContainsTooManyAsserts"
})
final class MjResolveTest {

    @Test
    void resolvesWithSingleDependency(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.7.0",
                "+version 0.25.0\n",
                "# No comments.",
                "[] > main ?"
            ).execute(new FakeMaven.Resolve());
        final Path path = temp
            .resolve("target")
            .resolve(MjResolve.DIR)
            .resolve("org.eolang/eo-runtime/-/0.7.0");
        MatcherAssert.assertThat(
            "Dependency directory must exist, but it doesn't",
            path.toFile(),
            FileMatchers.anExistingDirectory()
        );
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            path.resolve("eo-runtime-0.7.0.class").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void resolvesDefaultJnaDependency(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "Default JNA dependency must be resolved",
            new FakeMaven(temp)
                .withHelloWorld()
                .with("ignoreRuntime", true)
                .execute(new FakeMaven.Resolve())
                .result(),
            Matchers.hasKey(String.format("target/%s/net.java.dev.jna/jna/-/5.14.0", MjResolve.DIR))
        );
    }

    @Test
    void resolvesWithoutAnyDependencies(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "+package foo.x\n",
            "# No comments.",
            "[a b] > main",
            "  plus. > @",
            "    a",
            "    b"
        );
        maven.foreignTojos().add("sum");
        maven.execute(new FakeMaven.Resolve());
        final Path path = temp
            .resolve("target")
            .resolve(MjResolve.DIR)
            .resolve("org.eolang/eo-runtime/-/");
        MatcherAssert.assertThat(
            "The directory with runtime must exist, but doesn't",
            path.toFile(),
            FileMatchers.anExistingDirectory()
        );
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            path,
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }

    @Test
    void resolvesWithEoRuntimeDependency(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld().execute(new FakeMaven.Resolve());
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
            .execute(new FakeMaven.Resolve());
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
            "+version 0.25.0\n",
            "# No comments.",
            "[] > main"
        ).execute(new FakeMaven.Resolve());
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
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.22.1\n",
            "# Main.",
            "[] > main"
        ).with("ignoreRuntime", true).execute(new FakeMaven.Resolve());
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
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-0.7.0.class")
        );
    }

    @Test
    void resolvesWithoutTransitiveDependencies(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .with("ignoreTransitive", false)
            .with(
                "transitiveStrategy",
                (Func<Dep, Dependencies>) ignore -> new Dependencies.Fake(0)
            )
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }

    @Test
    void throwsExceptionWithTransitiveDependency(@Mktmp final Path temp) {
        final FakeMaven maven = new FakeMaven(temp);
        final Dependency dependency = new Dependency();
        dependency.setScope("compiled");
        dependency.setGroupId("org.eolang");
        dependency.setArtifactId("eo-transitive");
        dependency.setVersion("0.1.0");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven
                .withProgram(
                    "+rt jvm org.eolang:eo-foreign:0.22.1\n",
                    "[] > foo /int"
                )
                .with("ignoreTransitive", false)
                .with(
                    "transitiveStrategy",
                    (Func<Dependency, Iterable<Dependency>>) ignore -> Collections.singleton(
                        dependency
                    )
                )
                .execute(new FakeMaven.Resolve()),
            "Expected an IllegalStateException exception when transitive dependency"
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
            "+version 0.25.0\n",
            "# No comment.",
            "[] > main ?"
        ).withProgram(
            "+package foo.x",
            "+rt jvm org.eolang:eo-runtime:0.22.0",
            "+version 0.25.0\n",
            "# No comment.",
            "[] > main-1 ?"
        );
        final Exception except = Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Resolve())
        );
        MatcherAssert.assertThat(
            "Expected that conflicting dependencies were found, but they were not",
            except.getCause().getCause().getMessage(),
            Matchers.containsString(
                "1 conflicting dependencies are found: {org.eolang:eo-runtime:jar:=[0.22.0, 0.22.1]}"
            )
        );
    }

    @Test
    void resolvesWithConflictingDependenciesNoFail(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1\n",
                "# No comment.",
                "[] > main ?"
            ).withProgram(
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1\n",
                "# No comment.",
                "[] > main-1 ?"
            );
        maven.with("ignoreVersionConflicts", true)
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            "The class file must exist, but it doesn't",
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.class")
        );
    }
}
