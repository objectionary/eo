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

import java.io.File;
import java.nio.file.Path;
import java.util.function.BiConsumer;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.CommitHash;

/**
 * Copy source files from source directory with extra package on top.
 * This mojo is about to be used only on deployment step.
 * See more details
 * <a href="https://github.com/objectionary/eo/issues/2506#issuecomment-1759609269">here</a>
 *
 * @since 0.33.0
 */
@Mojo(
    name = "copy-sources",
    defaultPhase = LifecyclePhase.DEPLOY,
    threadSafe = true
)
public final class CopySourcesMojo extends SafeMojo {
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

    /**
     * Hash to deploy.
     * @todo #2506:30min Replace hash with tag or remove it at all. This mojo should be executed
     *  on "deploy" phase and copy sources with dynamic hash. The hash should be taken from tag
     *  that is provided with command `mvn versions:set "-DnewVersion=${tag}"` which is executed
     *  before actual `mvn deploy`. Now for the test purposes we can specify deployHash from
     *  pom.xml but it should be remade or removed in the future.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.deployHash",
        required = true
    )
    private String deployHash;

    @Override
    void exec() {
        final BiConsumer<Path, Path> copied = new CopiedResources(
            this.project,
            this.session,
            this.manager
        );
        copied.accept(
            this.javaSources.toPath(),
            this.generatedDir.toPath().resolve(
                new ChNarrow(new CommitHash.ChConstant(this.deployHash)).value()
            )
        );
        copied.accept(
            this.javaSources.toPath(),
            this.generatedDir.toPath()
        );
    }
}
