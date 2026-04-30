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
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * Runtime dependency downloaded from Maven Central.
 * @since 0.62.0
 */
final class RtCentral implements Scalar<Dep> {

    /**
     * Dependency downloaded by HTTP from Maven Central.
     */
    private static final Unchecked<Dep> MAVEN_DEPENDENCY = RtCentral.mavenDependency();

    @Override
    public Dep value() {
        return RtCentral.MAVEN_DEPENDENCY.value();
    }

    /**
     * Runtime dependency source from Maven Central.
     * @return Runtime dependency from Maven Central
     */
    private static Unchecked<Dep> mavenDependency() {
        final String url =
            "https://repo.maven.apache.org/maven2/org/eolang/eo-runtime/maven-metadata.xml";
        return RtCentral.dependency(() -> RtCentral.download(url));
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static String download(final String url) {
        try {
            return new Xnav(new XMLDocument(new URL(url)).inner())
                .element("metadata")
                .element("versioning")
                .element("latest")
                .text()
                .orElseThrow();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Can't get eo-runtime dependency by the URL: %s", url),
                ex
            );
        }
    }

    /**
     * Runtime dependency source.
     * @param version Version of eo-runtime
     * @return Maven Dependency
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
