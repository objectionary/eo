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
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.AbstractMap;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.OutputTo;
import org.cactoos.list.ListOf;
import org.eolang.parser.Xsline;

/**
 * Optimize XML files.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "optimize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class OptimizeMojo extends SafeMojo {

    /**
     * The directory where to place intermediary files.
     */
    public static final String STEPS = "02-steps";

    /**
     * The directory where to transpile to.
     */
    public static final String DIR = "03-optimize";

    @Override
    public void exec() throws IOException {
        final Map<String, Collection<String>> objects = new HashMap<>();
        final Collection<Tojo> sources = this.scopedTojos().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR)
        );
        int done = 0;
        for (final Tojo tojo : sources) {
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
            if (tojo.exists(AssembleMojo.ATTR_XMIR2)) {
                final Path tgt = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
                if (tgt.toFile().lastModified() >= src.toFile().lastModified()) {
                    Logger.debug(
                        this, "Already optimized %s to %s",
                        Save.rel(src), Save.rel(tgt)
                    );
                    continue;
                }
            }
            ++done;
            final Map.Entry<Path, Collection<String>> optimized = this.optimize(src);
            for (final String fqdn : optimized.getValue()) {
                objects.putIfAbsent(fqdn, new HashSet<>());
                objects.get(fqdn).add(Save.rel(optimized.getKey()));
            }
            tojo.set(
                AssembleMojo.ATTR_XMIR2,
                optimized.getKey().toAbsolutePath().toString()
            );
        }
        if (done > 0) {
            Logger.info(this, "Optimized %d out of %d XMIR program(s)", done, sources.size());
        } else {
            Logger.debug(this, "No XMIR programs out of %d optimized", sources.size());
        }
        this.checkForDuplicates(objects);
    }

    /**
     * Find duplicates in the current compilation scope.
     * @param objects Top-level compiled objects.
     */
    private void checkForDuplicates(final Map<String, Collection<String>> objects) {
        final Collection<String> duplicates = objects
            .entrySet()
            .stream()
            .filter(entry -> entry.getValue().size() > 1)
            .map(
                entry -> {
                    final String msg = String.format(
                        "object '%s' defined %d times (%s)",
                        entry.getKey(),
                        entry.getValue().size(),
                        String.join(", ", entry.getValue())
                    );
                    Logger.error(this, msg);
                    return msg;
                }
            ).collect(Collectors.toList());
        if (!duplicates.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "Found %d duplicates: %s",
                    duplicates.size(),
                    String.join(", ", duplicates)
                )
            );
        }
    }

    /**
     * Optimize XML file after parsing.
     *
     * @param file EO file
     * @return The file with optimized XMIR and top level objects.
     * @throws IOException If fails
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    private Map.Entry<Path, Collection<String>> optimize(final Path file) throws IOException {
        final String name = new XMLDocument(file).xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path dir = place.make(
            this.targetDir.toPath().resolve(OptimizeMojo.STEPS), ""
        );
        final Path target = place.make(
            this.targetDir.toPath().resolve(OptimizeMojo.DIR), Transpiler.EXT
        );
        new Xsline(
            new XMLDocument(file),
            new OutputTo(target),
            new TargetSpy(dir)
        ).with(
            new ListOf<>(
                "org/eolang/parser/optimize/globals-to-abstracts.xsl",
                "org/eolang/parser/optimize/remove-refs.xsl",
                "org/eolang/parser/optimize/abstracts-float-up.xsl",
                "org/eolang/parser/optimize/remove-levels.xsl",
                "org/eolang/parser/add-refs.xsl",
                "org/eolang/parser/optimize/fix-missed-names.xsl",
                "org/eolang/parser/add-refs.xsl",
                "org/eolang/parser/errors/broken-refs.xsl"
            )
        ).pass();
        final String pack = name.replaceFirst("[^.]+$", "");
        final List<String> atoms = new XMLDocument(target)
            .xpath("/program/objects/o[@name and not(@level) and not(@foreign)]/@name")
            .stream()
            .map(x -> String.format("%s%s", pack, x))
            .collect(Collectors.toList());
        Logger.debug(
            this, "Optimized %s (program:%s) to %s, all steps are in %s",
            Save.rel(file), name, Save.rel(target), Save.rel(dir)
        );
        return new AbstractMap.SimpleEntry<>(target, atoms);
    }

}
