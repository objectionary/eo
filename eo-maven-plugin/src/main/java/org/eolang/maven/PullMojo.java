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
import java.util.ArrayList;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnCached;
import org.eolang.maven.name.OnSwap;
import org.eolang.maven.name.OnVersioned;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.objectionary.ObjsDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Rel;

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
    private CommitHash hash;

    /**
     * Objectionaries.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Objectionaries objectionaries = new ObjsDefault(
        () -> this.cache,
        () -> this.session.getRequest().isUpdateSnapshots()
    );

    /**
     * Pull again even if the .eo file is already present?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.10.0
     */
    @Parameter(property = "eo.overWrite", required = true, defaultValue = "false")
    private boolean overWrite;

    /**
     * Pull objects from objectionaries or not.
     * @since 0.32.0
     */
    @Parameter(property = "eo.offline", required = true, defaultValue = "false")
    private boolean offline;

    @Override
    public void exec() throws IOException {
        if (this.offline) {
            Logger.info(
                this,
                "No programs were pulled because eo.offline flag is TRUE"
            );
        } else {
            if (this.hash == null) {
                this.hash = new ChCached(
                    new ChNarrow(
                        new ChRemote(this.tag)
                    )
                );
            }
            final Collection<ForeignTojo> tojos = this.scopedTojos().withoutSources();
            final Collection<ObjectName> names = new ArrayList<>(0);
            for (final ForeignTojo tojo : tojos) {
                final ObjectName name = new OnCached(
                    new OnSwap(
                        this.withVersions,
                        new OnVersioned(tojo.identifier(), this.hash)
                    )
                );
                names.add(name);
                tojo.withSource(this.pull(name).toAbsolutePath())
                    .withHash(new ChNarrow(name.hash()));
            }
            Logger.info(
                this,
                "%d program(s) were pulled: %s",
                tojos.size(),
                names
            );
        }
    }

    /**
     * Pull one object.
     * @param object Name of the object with/without version, e.g. "org.eolang.io.stdout|5f82cc1"
     * @return The path of .eo file
     * @throws IOException If fails
     */
    private Path pull(final ObjectName object) throws IOException {
        final Path dir = this.targetDir.toPath().resolve(PullMojo.DIR);
        final Path src = new Place(object).make(
            dir, "eo"
        );
        if (src.toFile().exists() && !this.overWrite) {
            Logger.debug(
                this, "The object '%s' already pulled to %s (and 'overWrite' is false)",
                object, new Rel(src)
            );
        } else {
            new HmBase(dir).save(
                this.objectionaries.object(object),
                dir.relativize(src)
            );
            Logger.debug(
                this, "The sources of the object '%s' pulled to %s",
                object, new Rel(src)
            );
        }
        return src;
    }
}
