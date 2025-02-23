/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * Add runtime dependency to the list of dependencies, if it is absent there.
 *
 * @since 0.28.11
 */
final class DcsWithRuntime implements Iterable<Dependency> {

    /**
     * Dependency downloaded by HTTP from Maven Central.
     */
    private static final Unchecked<Dependency> MAVEN_DEPENDENCY = DcsWithRuntime.mavenDependency();

    /**
     * All dependencies.
     */
    private final Iterable<Dependency> delegate;

    /**
     * Supplier of the eo-runtime dependency.
     */
    private final Unchecked<Dependency> supplied;

    /**
     * Constructor.
     *
     * @param dlg Dependencies delegate.
     */
    DcsWithRuntime(final Iterable<Dependency> dlg) {
        this(dlg, DcsWithRuntime.MAVEN_DEPENDENCY);
    }

    /**
     * Constructor.
     *
     * @param dlg Dependencies delegate.
     * @param sup Dependency.
     */
    DcsWithRuntime(
        final Iterable<Dependency> dlg,
        final Dependency sup
    ) {
        this(dlg, new Unchecked<>(() -> sup));
    }

    /**
     * The main constructor.
     *
     * @param dlg Dependencies delegate.
     * @param sup Supplier of the eo-runtime dependency.
     */
    DcsWithRuntime(
        final Iterable<Dependency> dlg,
        final Unchecked<Dependency> sup
    ) {
        this.delegate = dlg;
        this.supplied = sup;
    }

    @Override
    public Iterator<Dependency> iterator() {
        final ListOf<Dependency> all = new ListOf<>(this.delegate);
        if (all.stream().noneMatch(DcsWithRuntime::isRuntime)) {
            all.add(this.supplied.value());
        }
        return all.iterator();
    }

    /**
     * Runtime dependency source from Maven Central.
     *
     * @return Runtime dependency from Maven Central.
     */
    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static Unchecked<Dependency> mavenDependency() {
        final String url = String.format(
            "https://repo.maven.apache.org/maven2/%s/maven-metadata.xml",
            "org/eolang/eo-runtime"
        );
        try {
            return DcsWithRuntime.dependency(
                new Xnav(new XMLDocument(new URL(url)).inner())
                    .element("metadata")
                    .element("versioning")
                    .element("latest")
                    .text()
                    .get()
            );
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't get eo-runtime dependency by the URL: %s", url),
                ex
            );
        }
    }

    /**
     * Is it our runtime dep?
     * @param other The dep
     * @return TRUE if it is
     */
    private static boolean isRuntime(final Dependency other) {
        return "org.eolang".equals(other.getGroupId())
            && "eo-runtime".equals(other.getArtifactId());
    }

    /**
     * Runtime dependency source.
     *
     * @param version Version of eo-runtime
     * @return Maven Dependency.
     */
    private static Unchecked<Dependency> dependency(final String version) {
        return new Unchecked<>(
            new Synced<>(
                new Sticky<>(
                    () -> {
                        final Dependency dependency = new Dependency();
                        dependency.setGroupId("org.eolang");
                        dependency.setArtifactId("eo-runtime");
                        dependency.setVersion(version);
                        dependency.setClassifier("");
                        return dependency;
                    }
                )
            )
        );
    }
}
