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

import java.io.File;
import java.io.IOException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Abstact Mojo for all others.
 *
 * @since 0.1
 */
abstract class SafeMojo extends AbstractMojo {

    /**
     * Maven project.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    /**
     * Maven session.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(defaultValue = "${session}", readonly = true)
    protected MavenSession session;

    /**
     * Maven plugin manager.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Component
    protected BuildPluginManager manager;

    /**
     * File with foreign "tojos".
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(required = true, defaultValue = "${project.build.directory}/eo-foreign.csv")
    protected File foreign;

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @Parameter(required = true, defaultValue = "${project.build.directory}/eo")
    protected File targetDir;

    @Override
    public final void execute() throws MojoFailureException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        try {
            this.exec();
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Failed to execute %s",
                    this.getClass().getCanonicalName()
                ),
                ex
            );
        }
    }

    /**
     * Exec it.
     * @throws IOException If fails
     */
    abstract void exec() throws IOException;

}
