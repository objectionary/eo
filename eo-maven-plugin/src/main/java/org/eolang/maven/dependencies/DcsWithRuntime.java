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
package org.eolang.maven.dependencies;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * Add runtime dependency to the list of dependencies, if it is absent there.
 *
 * @since 0.28.11
 */
public final class DcsWithRuntime implements Iterable<Dependency> {

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
    public DcsWithRuntime(final Iterable<Dependency> dlg) {
        this(dlg, DcsWithRuntime.MAVEN_DEPENDENCY);
    }

    /**
     * Constructor.
     *
     * @param dlg Dependencies delegate.
     * @param sup Dependency.
     */
    public DcsWithRuntime(
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
    private static Unchecked<Dependency> mavenDependency() {
        final String url = String.format(
            "https://repo.maven.apache.org/maven2/%s/maven-metadata.xml",
            "org/eolang/eo-runtime"
        );
        try {
            return DcsWithRuntime.dependency(
                new XMLDocument(new URL(url)).xpath("//latest/text()").get(0)
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
        );
    }
}
