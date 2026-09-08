/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
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
 *
 * @since 0.55
 */
@ExtendWith(MktmpResolver.class)
final class CentralMavenTest {

    @Test
    @ExtendWith(WeAreOnline.class)
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
    @ExtendWith(WeAreOnline.class)
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
    void rejectsZipEntryEscapingDestination(@Mktmp final Path temp) throws IOException {
        final Path jar = temp.resolve("evil.jar");
        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(jar))) {
            zos.putNextEntry(new ZipEntry("org/eolang/ok.eo"));
            zos.write("ok".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
            zos.putNextEntry(new ZipEntry("../evil-escaped.txt"));
            zos.write("evil".getBytes(StandardCharsets.UTF_8));
            zos.closeEntry();
        }
        final Path dest = temp.resolve("dest");
        Assertions.assertThrows(
            IOException.class,
            () -> new UnpackedJar(jar, dest).unpack(),
            "A zip entry whose target lands outside the destination directory must be rejected"
        );
        MatcherAssert.assertThat(
            "The escaping entry must not be written outside the destination directory",
            dest.resolve("../evil-escaped.txt").normalize().toFile(),
            Matchers.not(FileMatchers.anExistingFile())
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
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
    @ExtendWith(WeAreOnline.class)
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
    @ExtendWith(WeAreOnline.class)
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

    @Test
    void composesWithAndThenInsteadOfThrowing() {
        MatcherAssert.assertThat(
            "andThen must return a composed BiConsumer, not throw at composition time",
            new CentralMaven().andThen(
                (dep, dest) -> {
                }
            ),
            Matchers.notNullValue()
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
