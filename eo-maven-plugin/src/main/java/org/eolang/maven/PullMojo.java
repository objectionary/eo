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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Supplier;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.CachePath;
import org.eolang.maven.footprint.Footprint;
import org.eolang.maven.footprint.FpFork;
import org.eolang.maven.footprint.FpGenerated;
import org.eolang.maven.footprint.FpIfReleased;
import org.eolang.maven.footprint.FpIfTargetExists;
import org.eolang.maven.footprint.FpIgnore;
import org.eolang.maven.footprint.FpUpdateBoth;
import org.eolang.maven.footprint.FpUpdateFromCache;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.tojos.ForeignTojo;

/**
 * Pull EO files from Objectionary.
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
    public static final String DIR = "4-pull";

    /**
     * Cache directory.
     */
    public static final String CACHE = "pulled";

    /**
     * The Git tag to pull objects from, in objectionary.
     * @since 0.21.0
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    private String tag = "master";

    /**
     * The Git hash to pull objects from, in objectionary.
     * If not set, will be computed from {@code tag} field.
     * @since 0.29.6
     */
    @SuppressWarnings("PMD.ImmutableField")
    private CommitHash hash = new ChCached(
        new ChRemote(this.tag)
    );

    /**
     * Objectionary.
     * @since 0.50
     * @checkstyle MemberNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Objectionary objectionary = new OyIndexed(
        new OyRemote(this.hash)
    );

    /**
     * Pull again even if the .eo file is already present?
     * @since 0.10.0
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.overWrite", required = true, defaultValue = "false")
    private boolean overWrite;

    @Override
    public void exec() throws IOException {
        if (this.offline) {
            Logger.info(
                this,
                "No programs were pulled because eo.offline flag is set to TRUE"
            );
        } else {
            this.pull();
        }
    }

    /**
     * Pull them all.
     * @throws IOException If fails
     */
    @SuppressWarnings("PMD.PrematureDeclaration")
    private void pull() throws IOException {
        final long start = System.currentTimeMillis();
        final Collection<ForeignTojo> tojos = this.scopedTojos().withoutSources();
        final Collection<String> names = new ArrayList<>(0);
        final Path base = this.targetDir.toPath().resolve(PullMojo.DIR);
        final String hsh = this.hash.value();
        for (final ForeignTojo tojo : tojos) {
            final String object = tojo.identifier();
            try {
                tojo.withSource(this.pulled(object, base, hsh))
                    .withHash(new ChNarrow(this.hash));
            } catch (final IOException exception) {
                throw new IOException(
                    String.format(
                        "Failed to pull '%s' earlier discovered at %s",
                        tojo.identifier(),
                        tojo.discoveredAt()
                    ),
                    exception
                );
            }
            names.add(object);
        }
        if (tojos.isEmpty()) {
            Logger.info(
                this,
                "No programs were pulled in %[ms]s",
                System.currentTimeMillis() - start
            );
        } else {
            Logger.info(
                this,
                "%d program(s) were pulled in %[ms]s: %s",
                tojos.size(),
                System.currentTimeMillis() - start,
                names
            );
        }
    }

    /**
     * Pull one object.
     * @param object Name of the object
     * @param base Base cache path
     * @param hsh Git hash
     * @return The path of .eo file
     * @throws IOException If fails
     */
    private Path pulled(final String object, final Path base, final String hsh) throws IOException {
        final String semver = this.plugin.getVersion();
        final Path target = new Place(object).make(base, AssembleMojo.EO);
        final Supplier<Path> che = new CachePath(
            this.cache.toPath().resolve(PullMojo.CACHE),
            semver,
            hsh,
            base.relativize(target)
        );
        final Footprint generated = new FpGenerated(
            src -> {
                Logger.debug(
                    this,
                    "Pulling %s object from remote objectionary with hash %s",
                    object, hsh
                );
                return new TextOf(this.objectionary.get(object)).asString();
            }
        );
        return new FpIfTargetExists(
            new FpFork(
                (src, tgt) -> {
                    final boolean rewrite = this.overWrite;
                    if (rewrite) {
                        Logger.debug(
                            this,
                            "Pulling sources again since eo.overWrite=TRUE"
                        );
                    }
                    return rewrite;
                },
                new FpIfReleased(
                    semver,
                    hsh,
                    new FpUpdateBoth(generated, che),
                    generated
                ),
                new FpIgnore()
            ),
            new FpIfReleased(
                semver,
                hsh,
                new FpIfTargetExists(
                    tgt -> che.get(),
                    new FpUpdateFromCache(che),
                    new FpUpdateBoth(generated, che)
                ),
                generated
            )
        ).apply(Paths.get(""), target);
    }
}
