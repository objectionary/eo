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
import java.util.List;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.Func;
import org.cactoos.experimental.Threads;
import org.cactoos.io.InputOf;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.footprint.FpDefault;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.TojoHash;
import org.eolang.parser.EoSyntax;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Parse EO to XML.
 *
 * @since 0.1
 */
@Mojo(
    name = "parse",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.ImmutableField")
public final class ParseMojo extends SafeMojo {

    /**
     * Zero version.
     */
    public static final String ZERO = "0.0.0";

    /**
     * The directory where to parse to.
     */
    public static final String DIR = "1-parse";

    /**
     * Subdirectory for parsed cache.
     */
    public static final String CACHE = "parsed";

    /**
     * The current version of eo-maven-plugin.
     * Maven 3 only.
     * You can read more about that property
     * <a href="https://maven.apache.org/plugin-tools/maven-plugin-tools-annotations/index.html#Supported_Annotations">here</a>.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(defaultValue = "${plugin}", readonly = true)
    private PluginDescriptor plugin;

    @Override
    public void exec() {
        final long start = System.currentTimeMillis();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> () -> this.parsed(tojo),
                    new Filtered<>(
                        ForeignTojo::notParsed,
                        this.scopedTojos().withSources()
                    )
                )
            )
        ).intValue();
        if (0 == total) {
            if (this.scopedTojos().withSources().isEmpty()) {
                Logger.info(
                    this,
                    "No .eo sources registered, nothing to be parsed to XMIRs (maybe you forgot to execute the \"register\" goal?)"
                );
            } else {
                Logger.info(this, "No new .eo sources parsed to XMIRs");
            }
        } else {
            Logger.info(
                this, "Parsed %d new .eo sources to XMIRs in %[ms]s",
                total, System.currentTimeMillis() - start
            );
        }
    }

    /**
     * Parse EO file to XML.
     *
     * @param tojo The tojo
     * @return Amount of parsed tojos
     * @throws IOException If fails
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.ExceptionAsFlowControl"})
    private int parsed(final ForeignTojo tojo) throws Exception {
        final Path source = tojo.source();
        final String name = tojo.identifier();
        final Path base = this.targetDir.toPath().resolve(ParseMojo.DIR);
        final Path target = new Place(name).make(base, AssembleMojo.XMIR);
        tojo.withXmir(
            new FpDefault(
                ParseMojo.parse(name),
                this.cache.resolve(ParseMojo.CACHE),
                this.plugin.getVersion(),
                new TojoHash(tojo),
                base.relativize(target)
            ).apply(source, target)
        );
        final List<XML> errors = new XMLDocument(target).nodes("/program/errors/error");
        if (errors.isEmpty()) {
            Logger.debug(this, "Parsed %[file]s to %[file]s", source, target);
        } else {
            for (final XML error : errors) {
                Logger.error(
                    this,
                    "Failed to parse '%[file]s:%s': %s",
                    source, error.xpath("@line").get(0), error.xpath("text()").get(0)
                );
            }
        }
        return 1;
    }

    /**
     * Function that parses EO source.
     * @param name Name of the EO object
     * @return Function that parses EO source
     */
    private static Func<Path, String> parse(final String name) {
        return source -> {
            final String parsed = new XMLDocument(
                new Xembler(
                    new Directives().xpath("/program").attr(
                        "source", source.toAbsolutePath()
                    )
                ).applyQuietly(new EoSyntax(name, new InputOf(source)).parsed().node())
            ).toString();
            Logger.debug(
                ParseMojo.class,
                "Parsed program %s:\n %s",
                name,
                parsed
            );
            return parsed;
        };
    }
}
