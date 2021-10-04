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
import java.io.File;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Func;
import org.cactoos.Input;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "assemble",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class AssembleMojo extends AbstractMojo {

    /**
     * The target folder.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    /**
     * The objectionary.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private Func<String, Input> objectionary = new Objectionary();

    @Override
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        int before = this.files();
        int cycle = 0;
        while (true) {
            new Moja<>(OptimizeMojo.class)
                .with("targetDir", this.targetDir)
                .execute();
            new Moja<>(PullMojo.class)
                .with("targetDir", this.targetDir)
                .with("objectionary", this.objectionary)
                .execute();
            final int after = this.files();
            if (after == before) {
                break;
            }
            ++cycle;
            Logger.info(
                this, "Assemble cycle #%d (%d -> %d)",
                cycle, before, after
            );
            before = after;
        }
        Logger.info(
            this, "%d assemble cycle(s) produced %d .eo.xml file(s)",
            cycle, before
        );
    }

    /**
     * How many files?
     * @return Total number
     * @throws MojoFailureException If fails
     */
    private int files() throws MojoFailureException {
        return new Walk(this.targetDir.toPath().resolve(OptimizeMojo.DIR)).size();
    }

}
