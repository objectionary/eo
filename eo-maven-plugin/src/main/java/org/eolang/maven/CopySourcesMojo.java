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

import java.nio.file.Path;
import java.util.function.BiConsumer;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import java.io.File;
import java.io.IOException;

/**
 * Copy source files from source directory with extra package on top.
 * This mojo is about to be used only on deployment step.
 * See more details
 * <a href="https://github.com/objectionary/eo/issues/2506#issuecomment-1759609269">here</a>
 */
@Mojo(
    name = "copySources",
    defaultPhase = LifecyclePhase.DEPLOY,
    threadSafe = true
)
public class CopySourcesMojo extends SafeMojo {
    /**
     * Directory with java sources.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.javaSources",
        required = true,
        defaultValue = "${project.basedir}/src/main/java"
    )
    private File javaSources;


    /**
     * Directory with generated sources.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.generatedDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources"
    )
    private File generatedDir;

    @Parameter(
        property = "deployHash",
        required = true
    )
    private String deployHash;

    @Override
    void exec() throws IOException {
        final BiConsumer<Path, Path> copied = new CopiedResources(
            this.project,
            this.session,
            this.manager
        );
        copied.accept(
            this.javaSources.toPath(),
            this.generatedDir.toPath().resolve(this.deployHash)
        );
        copied.accept(
            this.javaSources.toPath(),
            this.generatedDir.toPath()
        );
    }
}
