/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.yegor256.tojos.Tojos;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Set;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

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

    @Override
    public void exec() throws IOException {
        final int before = this.tojos.value().select(t -> true).size();
        if (before > 0) {
            Logger.info(this, "There are %d EO sources registered already", before);
        }
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath())
            .includes(this.includeSources)
            .excludes(this.excludeSources);
        final Unplace unplace = new Unplace(this.sourcesDir);
        for (final Path file : sources) {
            final String name = unplace.make(file);
            if (!this.scopedTojos().select(t -> t.get(Tojos.KEY).equals(name)).isEmpty()) {
                Logger.debug(this, "EO source %s already registered", name);
                continue;
            }
            this.scopedTojos()
                .add(name)
                .set(AssembleMojo.ATTR_VERSION, ParseMojo.ZERO)
                .set(AssembleMojo.ATTR_EO, file.toAbsolutePath().toString());
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
