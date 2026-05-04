/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.nio.file.Path;
import java.util.Collections;
import org.apache.maven.model.Dependency;
import org.apache.maven.repository.internal.MavenRepositorySystemUtils;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.repository.LocalRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.supplier.RepositorySystemSupplier;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CentralMaven}.
 * @since 0.55
 */
@ExtendWith({MktmpResolver.class, WeAreOnline.class})
final class CentralMavenTest {

    @Test
    void cachesArtifactInLocalRepositoryAfterDownload(@Mktmp final Path temp) {
        final Path local = temp.resolve("empty-local-repo");
        new CentralMaven(local).accept(
            CentralMavenTest.runtime(),
            temp.resolve("unpacked")
        );
        MatcherAssert.assertThat(
            "Artifact must be cached in the local repository after download",
            local.resolve("org/eolang/eo-runtime/0.7.0").toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    void unpacksFilesFromMavenCentral(@Mktmp final Path temp) {
        final Path dest = temp.resolve("unpacked");
        new CentralMaven(temp.resolve("empty-local-repo")).accept(
            CentralMavenTest.runtime(),
            dest
        );
        MatcherAssert.assertThat(
            "Unpacked destination must contain files fetched from Maven Central",
            dest.toFile().list(),
            Matchers.not(Matchers.emptyArray())
        );
    }

    @Test
    void resolvesWithDefaultConstructor(@Mktmp final Path temp) {
        final Path dest = temp.resolve("unpacked");
        new CentralMaven().accept(
            CentralMavenTest.runtime(),
            dest
        );
        MatcherAssert.assertThat(
            "Unpacked destination must contain files resolved using default constructor",
            dest.toFile().list(),
            Matchers.not(Matchers.emptyArray())
        );
    }

    @Test
    void throwsOnUnresolvableArtifact(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new CentralMaven(temp.resolve("local-repo")).accept(
                new Dep()
                    .withGroupId("org.eolang")
                    .withArtifactId("does-not-exist-xyz")
                    .withVersion("0.0.1")
                    .get(),
                temp.resolve("dest")
            ),
            "Must throw when artifact cannot be resolved"
        );
    }

    @Test
    void resolvesWithInjectedComponents(@Mktmp final Path temp) {
        final RepositorySystem system = new RepositorySystemSupplier().get();
        final DefaultRepositorySystemSession session = MavenRepositorySystemUtils.newSession();
        session.setLocalRepositoryManager(
            system.newLocalRepositoryManager(
                session,
                new LocalRepository(temp.resolve("local-repo").toFile())
            )
        );
        final Path dest = temp.resolve("unpacked");
        new CentralMaven(
            system,
            session,
            Collections.singletonList(
                new RemoteRepository.Builder(
                    "central", "default", "https://repo1.maven.org/maven2"
                ).build()
            )
        ).accept(
            CentralMavenTest.runtime(),
            dest
        );
        MatcherAssert.assertThat(
            "Unpacked destination must contain files when using injected resolver components",
            dest.toFile().list(),
            Matchers.not(Matchers.emptyArray())
        );
    }

    private static Dependency runtime() {
        return new Dep()
            .withGroupId("org.eolang")
            .withArtifactId("eo-runtime")
            .withVersion("0.7.0")
            .get();
    }
}
