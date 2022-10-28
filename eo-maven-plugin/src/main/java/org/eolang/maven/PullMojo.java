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
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.scalar.Ternary;

/**
 * Pull EO XML files from Objectionary and parse them into XML.
 *
 * @since 0.1
 */
@Mojo(
    name = "pull",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class PullMojo extends SafeMojo {

    /**
     * The directory where to process to.
     */
    public static final String DIR = "04-pull";

    /**
     * The Git hash to pull objects from, in objectionary.
     *
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.hash", required = true, defaultValue = "master")
    private String hash = "master";

    /**
     * Pull again even if the .eo file is already present?
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.10.0
     */
    @Parameter(property = "eo.overWrite", required = true, defaultValue = "false")
    private boolean overWrite;

    /**
     * Target directory.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.home")
    @SuppressWarnings("PMD.ImmutableField")
    private Path outputPath = Paths.get(System.getProperty("user.home")).resolve(".eo");

    /**
     * Read hashes from local file.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.offlineHashFile")
    private Path offlineHashFile;

    /**
     * The objectionary.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Objectionary objectionary;


    @Override
    public void exec() throws IOException {
        final Collection<Tojo> tojos = this.scopedTojos().select(
            row -> !row.exists(AssembleMojo.ATTR_EO)
                && !row.exists(AssembleMojo.ATTR_XMIR)
        );
        final CommitHash tag;
        if (offlineHashFile != null) {
            tag = new ChText(offlineHashFile, this.hash);
        } else {
            tag = new ChRemote(this.hash);
        }
        if (this.objectionary == null) {
            this.objectionary = new OyFallbackSwap(
                new OyHome(
                    new ChNarrow(tag),
                    this.outputPath
                ),
                new OyCaching(
                    new ChNarrow(tag),
                    this.outputPath,
                    PullMojo.remote(tag)
                ),
                this.forceUpdate()
            );
        }
        if (!tojos.isEmpty()) {
            for (final Tojo tojo : tojos) {
                tojo.set(
                    AssembleMojo.ATTR_EO,
                    this.pull(tojo.get(Tojos.KEY)).toAbsolutePath().toString()
                );
                tojo.set(
                    AssembleMojo.ATTR_HASH,
                    new ChNarrow(tag).value()
                );
            }
            Logger.info(
                this, "%d program(s) pulled from %s",
                tojos.size(), this.objectionary
            );
        }
    }

    /**
     * Create remote repo.
     *
     * @param hash Full Git hash
     * @return Objectionary
     */
    private static Objectionary remote(final CommitHash hash) {
        Objectionary obj;
        try {
            InetAddress.getByName("home.objectionary.com").isReachable(1000);
            obj = new OyRemote(hash);
        } catch (final IOException ex) {
            obj = new OyEmpty();
        }
        return obj;
    }

    /**
     * Is force update option enabled.
     *
     * @return True if option enabled and false otherwise
     */
    private boolean forceUpdate() {
        return this.session.getRequest().isUpdateSnapshots();
    }

    /**
     * Pull one object.
     *
     * @param name Name of the object, e.g. "org.eolang.io.stdout"
     * @return The path of .eo file
     * @throws IOException If fails
     */
    private Path pull(final String name) throws IOException {
        final Path src = new Place(name).make(
            this.targetDir.toPath().resolve(PullMojo.DIR), "eo"
        );
        if (src.toFile().exists() && !this.overWrite) {
            Logger.debug(
                this, "The object '%s' already pulled to %s (and 'overWrite' is false)",
                name, new Home().rel(src)
            );
        } else {
            new Home().save(
                this.objectionary.get(name),
                src
            );
            Logger.debug(
                this, "The sources of the object '%s' pulled to %s",
                name, new Home().rel(src)
            );
        }
        return src;
    }

}
