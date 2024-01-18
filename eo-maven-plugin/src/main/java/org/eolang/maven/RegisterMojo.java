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

import com.jcabi.log.Logger;
import java.io.File;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;

/**
 * Find and register all {@code .eo} sources in the "foreign" catalog.
 *
 * @since 0.12
 */
@Mojo(
    name = "register",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class RegisterMojo extends SafeMojo {

    /**
     * Directory in which {@code .eo} files are located.
     *
     * If you need to register {@code .eo} files located in many directories, you can
     * use the {@code &lt;includeSources&gt;} feature, for example:
     *
     * <pre> &lt;configuration&gt;
     *   &lt;sourcesDir&gt;/&lt;/sourcesDir&gt;
     *   &lt;includeSources&gt;
     *     &lt;glob&gt;tmp/&#42;&#42;/&#42;.eo&lt;/glob&gt;
     *     &lt;glob&gt;src/main/&#42;&#42;/&#42;.eo&lt;/glob&gt;
     *   &lt;/includeSources&gt;
     * &lt;/configuration&gt;</pre>
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.sourcesDir",
        required = true,
        defaultValue = "${project.basedir}/src/main/eo"
    )
    private File sourcesDir;

    /**
     * List of inclusion GLOB filters for finding EO files
     * in the {@code <includeSources>} directory, which can be
     * pretty global (or even a root one).
     * @implNote {@code property} attribute is omitted for collection
     * properties since there is no way of passing it via command line.
     * @checkstyle MemberNameCheck (15 lines)
     */
    @Parameter
    private Set<String> includeSources = new SetOf<>("**.eo");

    /**
     * List of exclusion GLOB filters for finding EO files
     * in the {@code &lt;includeSources&gt;} directory, which can be
     * pretty global (or even a root one).
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> excludeSources = new SetOf<>();

    /**
     * Whether it should fail on file names not matching required pattern.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.strictFileNames",
        required = true,
        defaultValue = "true"
    )
    private boolean strictFileNames = true;

    @Override
    public void exec() {
        if (this.sourcesDir == null) {
            throw new IllegalArgumentException(
                String.format("sourcesDir is null. Please specify a valid sourcesDir for %s", this)
            );
        }
        final Pattern pattern = Pattern.compile("^[a-zA-Z0-9\\-]+\\.eo$");
        final int before = this.scopedTojos().size();
        if (before > 0) {
            Logger.info(this, "There are %d EO sources registered already", before);
        }
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath())
            .includes(this.includeSources)
            .excludes(this.excludeSources);
        final Unplace unplace = new Unplace(this.sourcesDir);
        for (final Path file : sources) {
            if (this.strictFileNames
                && !pattern.matcher(file.getFileName().toString()).matches()) {
                throw new IllegalArgumentException(
                    String.format(
                        "Incorrect name found: '%s'. EO name must match '%s'",
                        file.getFileName().toString(),
                        pattern
                    )
                );
            }
            final String name = unplace.make(file);
            if (this.scopedTojos().contains(name)) {
                Logger.debug(this, "EO source %s already registered", name);
                continue;
            }
            this.scopedTojos()
                .add(name)
                .withSource(file.toAbsolutePath())
                .withVersion(ParseMojo.ZERO)
                .withVer(ParseMojo.ZERO);
            Logger.debug(this, "EO source %s registered", name);
        }
        Logger.info(
            this, "Registered %d EO sources from %s to %s, included %s, excluded %s",
            sources.size(), new Rel(this.sourcesDir),
            new Rel(this.foreign),
            this.includeSources, this.excludeSources
        );
    }
}
