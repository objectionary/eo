/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.maven.model.Dependency;
import org.apache.maven.repository.internal.MavenRepositorySystemUtils;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.repository.LocalRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.supplier.RepositorySystemSupplier;

/**
 * Downloads and unpacks a Maven artifact via the Maven Resolver (Aether) API.
 *
 * <p>Supports two modes:</p>
 * <ul>
 *   <li><b>Standalone</b> – use the no-arg constructor; a fresh
 *       {@link RepositorySystem} is built internally. Suitable for running
 *       outside a Maven build (tests, command-line tools, etc.).</li>
 *   <li><b>Maven-plugin</b> – pass the {@link RepositorySystem},
 *       {@link RepositorySystemSession}, and remote repositories that Maven
 *       already has configured. This avoids the
 *       {@code NoClassDefFoundError} for connector classes that arise when
 *       the plugin tries to bootstrap its own resolver inside Maven's
 *       classloader hierarchy.</li>
 * </ul>
 *
 * @since 0.55
 */
final class CentralMaven implements BiConsumer<Dependency, Path> {

    /**
     * Default local Maven repository path.
     */
    private static final Path LOCAL = Paths.get(
        System.getProperty("user.home"), ".m2", "repository"
    );

    /**
     * Maven Central URL.
     */
    private static final String CENTRAL = "https://repo1.maven.org/maven2";

    /**
     * Maven Resolver repository system.
     */
    private final RepositorySystem system;

    /**
     * Repository session.
     */
    private final RepositorySystemSession session;

    /**
     * Remote repositories to resolve from.
     */
    private final List<RemoteRepository> remotes;

    /**
     * Standalone constructor. Builds its own {@link RepositorySystem} backed
     * by the default local repository ({@code ~/.m2/repository}) and
     * Maven Central.
     */
    CentralMaven() {
        this(CentralMaven.LOCAL);
    }

    /**
     * Standalone constructor with a custom local repository.
     * @param local Path to the local Maven repository
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    CentralMaven(final Path local) {
        this(new RepositorySystemSupplier().get(), local);
    }

    /**
     * Constructor that accepts an optional repository system.
     * Falls back to a fresh standalone system when {@code sys} is {@code null},
     * which happens in tests that run without Maven injection.
     * @param sys Repository system, or {@code null} to build one automatically
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    CentralMaven(final RepositorySystem sys) {
        this(CentralMaven.nonNull(sys), CentralMaven.LOCAL);
    }

    /**
     * Private standalone constructor that builds the session from an already-created system.
     * @param sys Repository system
     * @param local Local repository path
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    private CentralMaven(final RepositorySystem sys, final Path local) {
        this(sys, CentralMaven.standaloneSession(sys, local));
    }

    /**
     * Private constructor that wires the standalone remotes.
     * @param sys Repository system
     * @param sess Repository session
     * @checkstyle ConstructorsCodeFreeCheck (10 lines)
     */
    private CentralMaven(final RepositorySystem sys, final DefaultRepositorySystemSession sess) {
        this(
            sys,
            sess,
            Collections.singletonList(
                new RemoteRepository.Builder("central", "default", CentralMaven.CENTRAL).build()
            )
        );
    }

    /**
     * Maven-plugin constructor. Reuses the already-configured components
     * provided by Maven's dependency-injection, avoiding classloader issues.
     * @param sys Repository system injected by Maven
     * @param sess Repository session injected by Maven
     * @param repos Remote repositories injected by Maven
     */
    CentralMaven(
        final RepositorySystem sys,
        final RepositorySystemSession sess,
        final List<RemoteRepository> repos
    ) {
        this.system = sys;
        this.session = sess;
        this.remotes = repos;
    }

    @Override
    public void accept(final Dependency dep, final Path dest) {
        final String classifier = Objects.requireNonNullElse(dep.getClassifier(), "");
        final Artifact artifact;
        try {
            artifact = this.system.resolveArtifact(
                this.session,
                new ArtifactRequest(
                    new DefaultArtifact(
                        dep.getGroupId(),
                        dep.getArtifactId(),
                        classifier,
                        "jar",
                        dep.getVersion()
                    ),
                    this.remotes,
                    null
                )
            ).getArtifact();
        } catch (final ArtifactResolutionException ex) {
            throw new IllegalStateException(
                String.format(
                    "Failed to resolve %s:%s:%s",
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion()
                ),
                ex
            );
        }
        Logger.info(
            this,
            "Resolved %s:%s:%s:%s to %s",
            dep.getGroupId(),
            dep.getArtifactId(),
            classifier,
            dep.getVersion(),
            artifact.getFile()
        );
        try {
            CentralMaven.unpack(artifact.getFile().toPath(), dest);
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Failed to unpack %s:%s:%s to %s",
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), dest
                ),
                ex
            );
        }
        if (classifier.isEmpty()) {
            Logger.info(
                this, "%s:%s:%s unpacked to %[file]s",
                dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), dest
            );
        } else {
            Logger.info(
                this, "%s:%s:%s:%s unpacked to %[file]s",
                dep.getGroupId(), dep.getArtifactId(), classifier, dep.getVersion(), dest
            );
        }
    }

    @Override
    public BiConsumer<Dependency, Path> andThen(
        final BiConsumer<? super Dependency, ? super Path> after
    ) {
        throw new UnsupportedOperationException("not implemented #andThen()");
    }

    /**
     * Returns the given system if non-null, otherwise creates a fresh one.
     * @param sys Repository system, or {@code null}
     * @return Non-null repository system
     */
    private static RepositorySystem nonNull(final RepositorySystem sys) {
        final RepositorySystem result;
        if (sys == null) {
            result = new RepositorySystemSupplier().get();
        } else {
            result = sys;
        }
        return result;
    }

    /**
     * Builds a standalone session for the given system and local repository.
     * @param sys Repository system
     * @param local Local repository path
     * @return Configured session
     */
    private static DefaultRepositorySystemSession standaloneSession(
        final RepositorySystem sys, final Path local
    ) {
        final DefaultRepositorySystemSession sess = MavenRepositorySystemUtils.newSession();
        sess.setLocalRepositoryManager(
            sys.newLocalRepositoryManager(sess, new LocalRepository(local.toFile()))
        );
        return sess;
    }

    /**
     * Unpacks a JAR (ZIP) file into the given directory.
     * @param jar Path to the JAR file
     * @param dest Destination directory
     * @throws IOException If unpacking fails
     */
    private static void unpack(final Path jar, final Path dest) throws IOException {
        Files.createDirectories(dest);
        try (ZipInputStream zis = new ZipInputStream(Files.newInputStream(jar))) {
            ZipEntry entry = zis.getNextEntry();
            while (entry != null) {
                final Path target = dest.resolve(entry.getName());
                if (entry.isDirectory()) {
                    Files.createDirectories(target);
                } else {
                    Files.createDirectories(target.getParent());
                    Files.copy(zis, target, StandardCopyOption.REPLACE_EXISTING);
                }
                zis.closeEntry();
                entry = zis.getNextEntry();
            }
        }
    }
}
