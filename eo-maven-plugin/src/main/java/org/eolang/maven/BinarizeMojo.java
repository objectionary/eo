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
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.rust.BuildFailureException;

/**
 * Compile binaries.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 * @todo #2195:90min Make cargo compilation parallel. Now cargo
 *  projects are being built consistently which is too long.
 *  It is much better to build them in parallel to reduce time.
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
        for (final File file: targetDir.toPath().resolve("Lib").toFile().listFiles()) {
            if (file.isDirectory() && file.toPath().resolve("Cargo.toml").toFile().exists()) {
                Logger.info(this, String.format("Building rust project.."));
                final ProcessBuilder builder = new ProcessBuilder("cargo", "build")
                    .directory(file);
                final Process building = builder.start();
                try {
                    building.waitFor();
                } catch (final InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new BuildFailureException(
                        String.format(
                            "Interrupted while building %s",
                            file
                        ),
                        exception
                    );
                }
                if (building.exitValue() != 0) {
                    Logger.error(this, "There was an error in compilation");
                    final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
                    try (VerboseProcess process = new VerboseProcess(building)) {
                        new Unchecked<>(
                            new LengthOf(
                                new TeeInput(
                                    new InputOf(process.stdoutQuietly()),
                                    new OutputTo(stdout)
                                )
                            )
                        ).value();
                    }
                    throw new BuildFailureException(
                        String.format(
                            "Failed to build cargo project with dest = %s",
                            file
                        )
                    );
                }
            }
        }
    }

}
