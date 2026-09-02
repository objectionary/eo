/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.Sources;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StXSL;
import java.io.IOException;
import java.nio.file.Path;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

/**
 * The step of the transpile train that runs {@code lowered.xsl}, which
 * renders every formation the {@code lower} goal folded into a Java atom
 * class, splicing the body of its {@code lambda()} method from a sidecar
 * file on disk.
 *
 * <p>It is not a {@link com.yegor256.xsline.StClasspath} for the same
 * reason {@link StPure} is not: where the files it reads are. Saxon routes
 * {@code unparsed-text()} through the resolver of the transformation, and
 * the one {@link com.yegor256.xsline.StClasspath} installs looks for every
 * URI on the classpath, where the sidecars of a build under way are not.
 * So the resolver here answers a {@code file:} URI from the disk and
 * leaves everything else — the libraries the stylesheet
 * {@code xsl:import}-s — to the classpath, as before. A sidecar that is
 * not there stays not there: {@code unparsed-text-available()} in the
 * stylesheet hears it and stops the build, since a {@code lowered} stamp
 * without its sidecar is an invariant violation.</p>
 *
 * @since 0.76.0
 */
final class StLowered extends StEnvelope {

    /**
     * Ctor.
     * @param sheet The classpath path of the stylesheet to run
     * @param atoms The directory with the sidecar bodies, which does not
     *  have to exist as long as nothing in the XMIR is marked as lowered
     */
    StLowered(final String sheet, final Path atoms) {
        super(new StXSL(StLowered.compiled(sheet, atoms)));
    }

    private static XSL compiled(final String sheet, final Path atoms) {
        try {
            return new XSLDocument(StLowered.class.getResource(sheet), sheet)
                .with(StLowered.sources())
                .with("disclaimer", new Disclaimer().toString())
                .with("sidecars", atoms.toUri().toString());
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Failed to read '%s' from classpath", sheet), ex
            );
        }
    }

    private static Sources sources() {
        final Sources classpath = new ClasspathSources();
        return (href, base) -> {
            final Source found;
            if (href.startsWith("file:")) {
                found = new StreamSource(href);
            } else {
                found = classpath.resolve(href, base);
            }
            return found;
        };
    }
}
