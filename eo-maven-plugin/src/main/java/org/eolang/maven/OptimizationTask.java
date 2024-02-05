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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.cactoos.BiFunc;
import org.cactoos.Func;
import org.cactoos.Scalar;
import org.eolang.maven.optimization.OptCached;
import org.eolang.maven.optimization.Optimization;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Rel;

/**
 * Optimized Tojos.
 * @since 0.34.0
 */
final class OptimizationTask {

    /**
     * The map with optimization folder paths of Mojo.
     */
    private final Map<String, Path> paths;

    /**
     * The map with target directory and EO cache directory of Mojo.
     */
    private final Map<String, String> dirs;

    /**
     * The function that get ForeignTojo of Mojo.
     */
    private final BiFunc<ForeignTojo, Path, ForeignTojo> update;

    /**
     * The function that get Tojo path of Mojo.
     */
    private final Func<ForeignTojo, Path> source;

    /**
     * Ctor.
     * @param paths Map with optimization folder paths of Mojo.
     * @param dirs Map with target directory and EO cache directory of Mojo.
     * @param update Function that updates ForeignTojo.
     * @param source Function that gets Tojo path.
     * @checkstyle ParameterNumberCheck (15 lines)
     */
    OptimizationTask(
        final Map<String, Path> paths,
        final Map<String, String> dirs,
        final BiFunc<ForeignTojo, Path, ForeignTojo> update,
        final Func<ForeignTojo, Path> source
    ) {
        this.paths = paths;
        this.dirs = dirs;
        this.update = update;
        this.source = source;
    }

    /**
     * Converts tojo to optimization task.
     *
     * @param tojo Tojo that should be optimized.
     * @param common Optimization.
     * @return Optimization task.
     * @throws Exception If fail during optimization.
     */
    public Scalar<Integer> value(
        final ForeignTojo tojo,
        final Optimization common
    ) throws Exception {
        final Path src = this.source.apply(tojo);
        Logger.debug(
            this, "Adding optimization task for %s",
            src
        );
        return () -> {
            this.update.apply(
                tojo,
                this.make(
                    this.optimization(tojo, common).apply(new XMLDocument(src)), src
                ).toAbsolutePath()
            );
            return 1;
        };
    }

    /**
     * Make a path with optimized XML file after parsing.
     *
     * @param xml Optimized xml
     * @param file EO file
     * @return The file with optimized XMIR
     * @throws IOException If fails
     */
    private Path make(final XML xml, final Path file) throws IOException {
        final String name = new XMLDocument(file).xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path dir = this.paths.get(OptimizationFolder.TARGET.key());
        final Path target = place.make(
            dir.resolve(this.dirs.get(OptimizationFolder.TARGET.key())), TranspileMojo.EXT
        );
        new HmBase(dir).save(
            xml.toString(),
            dir.relativize(target)
        );
        Logger.debug(
            this, "Optimized %s (program:%s) to %s",
            new Rel(file), name, new Rel(target)
        );
        return target;
    }

    /**
     * Optimization for specific tojo.
     *
     * @param tojo Tojo
     * @param common Optimization
     * @return Optimization for specific Tojo
     * @throws Exception If fails
     */
    private Optimization optimization(final ForeignTojo tojo, final Optimization common)
        throws Exception {
        final Optimization res;
        if (tojo.hasHash()) {
            res = new OptCached(
                common,
                this.paths.get(
                    OptimizationFolder.CACHE.key()
                ).resolve(this.dirs.get(OptimizationFolder.CACHE.key())).resolve(tojo.hash()),
                this.source.apply(tojo)
            );
        } else {
            res = common;
        }
        return res;
    }
}

