/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
import java.util.function.Supplier;
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
final class DpsWithRuntime implements Dependencies {

    /**
     * Dependency downloaded by HTTP from Maven Central.
     */
    private static final Unchecked<Dep> MAVEN_DEPENDENCY = DpsWithRuntime.mavenDependency();

    /**
     * All dependencies.
     */
    private final Iterable<Dep> delegate;

    /**
     * Supplier of the eo-runtime dependency.
     */
    private final Unchecked<Dep> supplied;

    /**
     * Constructor.
     *
     * @param dlg Dependencies delegate.
     */
    DpsWithRuntime(final Iterable<Dep> dlg) {
        this(dlg, DpsWithRuntime.MAVEN_DEPENDENCY);
    }

    /**
     * Constructor.
     *
     * @param dlg Dependencies delegate.
     * @param sup Dependency.
     */
    DpsWithRuntime(
        final Iterable<Dep> dlg,
        final Dep sup
    ) {
        this(dlg, new Unchecked<>(() -> sup));
    }

    /**
     * The main constructor.
     *
     * @param dlg Dependencies delegate.
     * @param sup Supplier of the eo-runtime dependency.
     */
    DpsWithRuntime(
        final Iterable<Dep> dlg,
        final Unchecked<Dep> sup
    ) {
        this.delegate = dlg;
        this.supplied = sup;
    }

    @Override
    public Iterator<Dep> iterator() {
        final ListOf<Dep> all = new ListOf<>(this.delegate);
        if (all.stream().noneMatch(dep -> DpsWithRuntime.isRuntime(dep.get()))) {
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
    private static Unchecked<Dep> mavenDependency() {
        final String url = String.format(
            "https://repo.maven.apache.org/maven2/%s/maven-metadata.xml",
            "org/eolang/eo-runtime"
        );
        return DpsWithRuntime.dependency(
            () -> {
                try {
                    return new Xnav(new XMLDocument(new URL(url)).inner())
                        .element("metadata")
                        .element("versioning")
                        .element("latest")
                        .text()
                        .get();
                } catch (final IOException ex) {
                    throw new IllegalStateException(
                        String.format("Can't get eo-runtime dependency by the URL: %s", url),
                        ex
                    );
                }
            }
        );
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
    private static Unchecked<Dep> dependency(final Supplier<String> version) {
        return new Unchecked<>(
            new Synced<>(
                new Sticky<>(
                    () -> new Dep()
                        .withGroupId("org.eolang")
                        .withArtifactId("eo-runtime")
                        .withVersion(version.get())
                        .withClassifier("")
                )
            )
        );
    }
}
