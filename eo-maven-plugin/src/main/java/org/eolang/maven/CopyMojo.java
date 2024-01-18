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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;

/**
 * Copy all .eo files from src/main/eo to target/classes/EO-SOURCES
 * and replace 0.0.0 versions in them to the right numbers.
 *
 * @since 0.11
 */
@Mojo(
    name = "copy",
    defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class CopyMojo extends SafeMojo {

    /**
     * Dir with sources.
     */
    public static final String DIR = "EO-SOURCES";

    /**
     * Replacer or version.
     */
    private static final Pattern REPLACE = Pattern.compile(
        "^(\\+rt .+):0\\.0\\.0(.*)$",
        Pattern.MULTILINE
    );

    /**
     * Directory in which .eo files are located.
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
     * Target directory with resources to be packaged in JAR.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.outputDir",
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDir;

    /**
     * The version to use for 0.0.0 replacements.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.version",
        required = true,
        defaultValue = "${project.version}"
    )
    private String version;

    @Override
    public void exec() throws IOException {
        final Path target = this.outputDir.toPath().resolve(CopyMojo.DIR);
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath());
        for (final Path src : sources) {
            new HmBase(target).save(
                CopyMojo.REPLACE
                    .matcher(new UncheckedText(new TextOf(new InputOf(src))).asString())
                    .replaceAll(String.format("$1:%s$2", this.version)),
                Paths.get(
                    src.toAbsolutePath().toString().substring(
                        this.sourcesDir.toPath().toAbsolutePath().toString().length() + 1
                    )
                )
            );
        }
        if (sources.isEmpty()) {
            Logger.warn(
                this, "No sources copied from %s to %s",
                new Rel(this.sourcesDir), new Rel(target)
            );
        } else {
            Logger.info(
                this, "%d source(s) copied from %s to %s",
                sources.size(), new Rel(this.sourcesDir),
                new Rel(target)
            );
        }
    }

}
