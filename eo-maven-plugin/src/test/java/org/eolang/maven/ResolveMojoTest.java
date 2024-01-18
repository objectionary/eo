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

import com.yegor256.WeAreOnline;
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
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ResolveMojo}.
 *
 * @since 0.1
 */
@ExtendWith(WeAreOnline.class)
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
final class ResolveMojoTest {

    @Test
    void resolvesWithSingleDependency(@TempDir final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                String.format(
                    "%s\n\n%s",
                    "+rt jvm org.eolang:eo-runtime:0.7.0",
                    "[] > foo /int"
                )
            ).execute(new FakeMaven.Resolve());
        final Path path = temp
            .resolve("target")
            .resolve(ResolveMojo.DIR)
            .resolve("org.eolang/eo-runtime/-/0.7.0");
        MatcherAssert.assertThat(path.toFile(), FileMatchers.anExistingDirectory());
        MatcherAssert.assertThat(
            path.resolve("eo-runtime-0.7.0.jar").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void resolvesWithoutAnyDependencies(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "[a b] > sum",
            "  plus. > @",
            "    a",
            "    b"
        );
        maven.foreignTojos().add("sum").withDiscovered(0);
        maven.execute(new FakeMaven.Resolve());
        final Path path = temp
            .resolve("target")
            .resolve(ResolveMojo.DIR)
            .resolve("org.eolang/eo-runtime/-/");
        MatcherAssert.assertThat(path.toFile(), FileMatchers.anExistingDirectory());
        MatcherAssert.assertThat(
            path,
            new ContainsFiles("**/eo-runtime-*.jar")
        );
    }

    @Test
    void resolvesWithEoRuntimeDependency(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld().execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.jar")
        );
    }

    @Test
    void resolvesWithoutEoRuntimeDependency(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .with("withRuntimeDependency", false)
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.jar"))
        );
    }

    @Test
    void resolvesIfRuntimeDependencyComesFromTojos(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .withProgram("+rt jvm org.eolang:eo-runtime:0.22.1", "", "[] > main")
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-0.22.1.jar")
        );
    }

    @Test
    void resolvesIfRuntimeDependencyComesFromTojosButParamIsFalse(@TempDir final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .withProgram("+rt jvm org.eolang:eo-runtime:0.22.1", "", "[] > main")
            .with("withRuntimeDependency", false)
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            Matchers.not(new ContainsFiles("**/eo-runtime-*.jar"))
        );
    }

    @Test
    void resolvesWithRuntimeDependencyFromPom(@TempDir final Path temp) throws IOException {
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
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-0.7.0.jar")
        );
    }

    @Test
    void resolvesWithoutTransitiveDependencies(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.withHelloWorld()
            .with("ignoreTransitive", false)
            .with(
                "transitiveStrategy",
                (Func<Dependency, Iterable<Dependency>>) ignore -> Collections.emptyList()
            )
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.jar")
        );
    }

    @Test
    void throwsExceptionWithTransitiveDependency(@TempDir final Path temp) {
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
                .execute(new FakeMaven.Resolve())
        );
    }

    /**
     * Test conflicts.
     *
     * @param temp Temp folder
     * @throws IOException In case of I/O issues.
     */
    @Test
    void resolvesWithConflictingDependencies(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.1",
                "[] > foo /int"
            )
        ).withProgram(
            String.format(
                "%s\n\n%s",
                "+rt jvm org.eolang:eo-runtime:0.22.0",
                "[] > foo /int"
            )
        );
        final Exception excpt = Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Resolve())
        );
        MatcherAssert.assertThat(
            excpt.getCause().getCause().getMessage(),
            Matchers.containsString(
                "1 conflicting dependencies are found: {org.eolang:eo-runtime:jar:=[0.22.0, 0.22.1]}"
            )
        );
    }

    @Test
    void resolvesWithConflictingDependenciesNoFail(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                String.format(
                    "%s\n\n%s",
                    "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.1",
                    "[] > foo /int"
                )
            ).withProgram(
                String.format(
                    "%s\n\n%s",
                    "+rt jvm org.eolang:eo-runtime:jar-with-dependencies:0.22.0",
                    "[] > foo /int"
                )
            );
        maven.with("ignoreVersionConflicts", true)
            .execute(new FakeMaven.Resolve());
        MatcherAssert.assertThat(
            maven.targetPath(),
            new ContainsFiles("**/eo-runtime-*.jar")
        );
    }
}
