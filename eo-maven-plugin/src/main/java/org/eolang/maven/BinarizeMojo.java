/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import com.jcabi.log.VerboseProcess;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.FileUtils;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.rust.BuildFailureException;

/**
 * Compile binaries.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 * @todo #2197:45min Update cached rust insert if it was changed.
 *  Now it copies cargo project to cache directory in the end of every
 *  compilation. It is better to copy the project only if it was changed
 *  with the last compilation.
 */
@Mojo(
    name = "binarize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.LongVariable")
public final class BinarizeMojo extends SafeMojo {

    /**
     * The directory where to binarize to.
     */
    public static final Path DIR = Paths.get("binarize");

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo-binaries"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File generatedDir;

    @Override
    public void exec() throws IOException {
        new Moja<>(BinarizeParseMojo.class).copy(this).execute();
        final int total = new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    project -> () -> {
                        final int built;
                        if (
                            project.isDirectory() &&
                                project.toPath().resolve("Cargo.toml").toFile().exists()
                        ) {
                            this.build(project);
                            built = 1;
                        } else {
                            built = 0;
                        }
                        return built;
                    },
                    targetDir.toPath().resolve("Lib").toFile().listFiles()
                )
            )
        ).intValue();
        Logger.info(
            this,
            String.format("Built in total %d cargo projects", total)
        );
    }

    /**
     * Builds cargo project.
     * @param project Path to the project.
     * @throws IOException If any issues with IO.
     */
    private void build(final File project) throws IOException {
        final File target = project.toPath().resolve("target").toFile();
        final File cached = cache
            .resolve("Lib")
            .resolve(project.getName())
            .resolve("target").toFile();
        if (cached.exists()) {
            Logger.info(
                this,
                String.format(
                    "Copying %s to %s",
                    cached,
                    target
                )
            );
            FileUtils.copyDirectory(
                cached,
                target
            );
        }
        Logger.info(this, "Building rust project..");
        try (
            VerboseProcess proc = new VerboseProcess(
                new ProcessBuilder("cargo", "build")
                    .directory(project)
            )
        ) {
            proc.stdout();
        } catch (final IllegalArgumentException exc) {
            throw new BuildFailureException(
                String.format(
                    "Failed to build cargo project with dest = %s",
                    project
                ),
                exc
            );
        }
        Logger.info(
            this,
            String.format(
                "Cargo building succeeded, update cached %s with %s",
                cached,
                target
            )
        );
        FileUtils.copyDirectory(project, cached);
    }
}
