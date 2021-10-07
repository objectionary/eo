/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.func.IoCheckedFunc;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojo;
import org.eolang.tojos.Tojos;

/**
 * Pull EO XML files from Objectionary and parse them into XML.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
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
     * Pull again even if the .eo file is already present?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.10.0
     */
    @Parameter(required = true, defaultValue = "false")
    private boolean overWrite;

    /**
     * The objectionary.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Func<String, Input> objectionary = new Objectionary();

    @Override
    public void exec() throws IOException {
        final Tojos tojos = new MonoTojos(this.foreign);
        final Collection<Tojo> fobjs = tojos.select(
            row -> !row.exists("eo") && !row.exists("xmir")
        );
        if (!fobjs.isEmpty()) {
            Logger.info(this, "%d EO objects found", fobjs.size());
            for (final Tojo tojo : fobjs) {
                tojo.set(
                    "eo",
                    this.pull(tojo.get("id")).toAbsolutePath().toString()
                );
            }
        }
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
                name, src
            );
        } else {
            new Save(
                new IoCheckedFunc<>(this.objectionary).apply(name),
                src
            ).save();
            Logger.info(this, "The sources of the object '%s' pulled to %s", name, src);
        }
        return src;
    }

}
